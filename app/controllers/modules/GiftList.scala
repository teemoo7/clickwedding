package controllers.modules

import controllers.AnyController
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.i18n.Messages
import anorm.{Id, Pk, NotAssigned}
import play.api.mvc.Action
import play.api.{Logger, Routes}
import play.api.cache.Cache
import play.api.Play.current
import collection.mutable
import models.modules.{GiftListPayPalInfo, CartItem}
import play.api.libs.json.Json._
import views.html.helper.currency
import play.api.libs.ws.WS
import controllers.helpers.WSHelper._
import controllers.helpers.UtilsHelper
import play.api.libs.concurrent.Execution.Implicits._
import play.api.data.format.Formats._
import controllers.admin.NotificationCenter
import java.util.Date

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 30.01.13
 * Time: 08:42
 * To change this template use File | Settings | File Templates.
 */
object GiftList extends AnyController {
  val CART_CACHE_KEY = "cart"
  val PURCHASE_CACHE_KEY = "PayPalExpressCheckoutPurchase"

  val CURRENCY_CHF = "CHF"

  /**
   * Gift list edition form definition
   */
  val giftListInfoForm: Form[models.modules.GiftListInfo] = Form(
    mapping(
      "email" -> optional(email).verifying(Messages("error.maxLength", 150), o => o.getOrElse("").length <= 150)
    )
      ((email) => models.modules.GiftListInfo(null, null, email, CURRENCY_CHF, mutable.HashMap()))
      ((giftListInfo: models.modules.GiftListInfo) => Some(giftListInfo.email))
  )

  /**
   * Gift list instructions options form definition
   */
  val giftListInstructionsForm = Form(
    "instructions" -> optional(text(minLength = 0, maxLength = 300))
  )

  /**
   * New gift item form definition
   */
  val addGiftListItemForm: Form[models.modules.GiftListItem] = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "description" -> nonEmptyText(maxLength = 150),
      "unitPrice" -> of[Double].verifying(price => price > 0.0),
      "stock" -> optional(number(min = 0))
    )
      ((id, weddingId, description, unitPrice, stock) => models.modules.GiftListItem(id, weddingId, description, unitPrice, stock, true))
      ((giftListItem: models.modules.GiftListItem) => Some(giftListItem.id, giftListItem.weddingId, giftListItem.description, giftListItem.unitPrice, giftListItem.stock))
  )

  /**
   * Edit gift item form definition
   */
  val editGiftListItemForm: Form[models.modules.GiftListItem] = Form(
    mapping(
      "id" -> number,
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "description" -> nonEmptyText(maxLength = 150),
      "unitPrice" -> of[Double].verifying(price => price > 0.0),
      "stock" -> optional(number(min = 0))
    )
      ((id, weddingId, description, unitPrice, stock) => models.modules.GiftListItem(Id(id), weddingId, description, unitPrice, stock, true))
      ((giftListItem: models.modules.GiftListItem) => Some(giftListItem.id.get.toInt, giftListItem.weddingId, giftListItem.description, giftListItem.unitPrice, giftListItem.stock))
  )

  /**
   * Checkout form definition
   */
  val checkoutForm = Form(
    tuple(
      "name" -> nonEmptyText,
      "email" -> email.verifying(Messages("error.maxLength", 150), e => e.length <= 150),
      "message" -> optional(text(maxLength = 200)),
      "conditions" -> checked(Messages("main.payment.pleaseAcceptTermsAndConditions"))
    )
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.GiftList.addItem,
        routes.javascript.GiftList.editItem,
        routes.javascript.GiftList.deleteItem,
        routes.javascript.GiftList.addToCart,
        routes.javascript.GiftList.removeFromCart,
        routes.javascript.GiftList.checkPayPal
      )
    ).as("text/javascript")
  }

  /**
   * Display the gift list
   * @param uid the wedding UID
   */
  def display(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              val cart = Cache.get(getCartCacheKey(wedding)) match {
                case Some(cartCache: models.modules.Cart) => cartCache
                case _ => models.modules.Cart(mutable.HashMap())
              }
              Cache.set(getCartCacheKey(wedding), cart)
              Ok(views.html.modules.giftList.display(wedding, giftList, cart))
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }


  /**
   * Handle AJAX gift addition to cart
   * @param uid the wedding UID
   */
  def addToCart(uid: String, itemId: Long) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              giftList.moduleContent.items.get(itemId) match {
                case Some(item) => {
                  Cache.get(getCartCacheKey(wedding)) match {
                    case Some(cart: models.modules.Cart) => {
                      cart.items.get(itemId) match {
                        case Some(cartItem) =>
                          cart.items.put(itemId, cartItem.copy(number = cartItem.number+1))
                        case None =>
                          cart.items.put(itemId, CartItem(item, 1))
                      }
                      Ok(toJson(Map(
                        "line" -> views.html.modules.giftList.cartItem(cart.items.get(itemId).get).toString,
                        "totalPrice" -> currency(cart.items.values.map(_.total).sum).toString,
                        "totalNumber" -> cart.items.values.map(_.number).sum.toString
                      )))
                    }
                    case _ =>
                      NotFound
                  }
                }
                case _ =>
                  NotFound
              }
            case _ =>
              NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX gift deletion to cart
   * @param uid the wedding UID
   * @param itemId the item ID
   */
  def removeFromCart(uid: String, itemId: Long) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              giftList.moduleContent.items.get(itemId) match {
                case Some(item) => {
                  Cache.get(getCartCacheKey(wedding)) match {
                    case Some(cart: models.modules.Cart) => {
                      cart.items.get(itemId) match {
                        case Some(cartItem) =>
                          val newCartItem = cartItem.copy(number = cartItem.number-1)
                          val html = newCartItem.number > 0 match {
                            case true => {
                              cart.items.put(itemId, newCartItem)
                              views.html.modules.giftList.cartItem(cart.items.get(itemId).get).toString()
                            }
                            case false => {
                              cart.items.remove(itemId)
                              ""
                            }
                          }
                          Ok(toJson(Map(
                            "line" -> html,
                            "totalPrice" -> currency(cart.items.values.map(_.total).sum).toString,
                            "totalNumber" -> cart.items.values.map(_.number).sum.toString
                          )))
                        case None =>
                          Ok
                      }
                    }
                    case _ =>
                      NotFound
                  }
                }
                case _ =>
                  NotFound
              }
            case _ =>
              NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  def pay(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              Cache.get(getCartCacheKey(wedding)) match {
                case Some(cart: models.modules.Cart) => {
                  setExpressCheckout(cart, giftList, wedding)
                }
                case _ => BadRequest
              }
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Callback URL when after the user filled the PayPal checkout. Display the confirmation page to pay now.
   * @param uid the wedding UID
   */
  def checkout(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          Cache.get(PURCHASE_CACHE_KEY) match {
            case Some(purchase: models.modules.GiftListPurchase) => {
              val params = parseParameters(request.rawQueryString)
              val token = params.get("token")
              val payerId = params.get("PayerID")
              purchase.token match {
                case Some(t) => {
                  t.equals(token.getOrElse("")) match {
                    case true => {
                      val checkoutPurchase = purchase.copy(payerId = payerId)
                      Cache.set(PURCHASE_CACHE_KEY, checkoutPurchase, 15*60)
                      Ok(views.html.modules.giftList.checkout(wedding, checkoutPurchase, checkoutForm))
                    }
                    case false => {
                      Logger.error("ExpressCheckout gift list data fill by user has failed (wrong token): "+params.toSeq.toString+" / "+purchase.toString);
                      goToGiftListDisplay(uid).flashing("error" -> Messages("main.payment.wrongToken"))
                    }
                  }
                }
                case None => {
                  Logger.error("ExpressCheckout gift list data fill by user has failed (no token in Payment): "+params.toSeq.toString+" / "+purchase.toString);
                  goToGiftListDisplay(uid).flashing("error" -> Messages("main.payment.noTokenInPayment"))
                }
              }
            }
            case _ => {
              Logger.error("ExpressCheckout gift list data fill by user has failed: timeout.");
              goToGiftListDisplay(uid).flashing("error" -> Messages("main.payment.errorTimeOut"))
            }
          }
        }
      }.getOrElse(NotFound)

  }

  /**
   * Handle payment confirmation form submission
   * @param uid the wedding UID
   */
  def doPay(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) => {
              Cache.get(PURCHASE_CACHE_KEY) match {
                case Some(purchase: models.modules.GiftListPurchase) => {
                  checkoutForm.bindFromRequest.fold(
                    formWithErrors =>
                      BadRequest(views.html.modules.giftList.checkout(wedding, purchase, formWithErrors)),
                    checkout => {
                      doExpressCheckout(wedding, purchase.copy(name = checkout._1, email = Some(checkout._2), message = checkout._3), giftList)
                    }
                  )
                }
                case _ => {
                  Logger.error("ExpressCheckout gift list confirmation by guest has failed: timeout.");
                  goToGiftListDisplay(uid).flashing("error" -> Messages("main.payment.errorTimeOut"))
                }
              }
            }
            case _ => NotFound
          }
      }.getOrElse(NotFound)
  }

  /**
   * Display the invoice of the given purchase
   * @param uid the wedding UID
   * @param purchaseId the purchase ID
   * @param transactionId the PayPal transaction ID
   */
  def displayInvoice(uid: String, purchaseId: Long, transactionId: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              models.modules.GiftList.getGiftListPurchase(wedding.id, purchaseId).map {
                purchase => purchase.transactionId.getOrElse("").equals(transactionId) match {
                  case true =>
                    Ok(views.html.modules.giftList.mailInvoice(purchase, giftList.moduleContent.paypalInfo, wedding))
                  case false =>
                    NotFound
                }
              }.getOrElse(NotFound)
            case _ => NotFound
          }
      }.getOrElse(NotFound)
  }

  /**
   * Display the admin console for the gift list
   * @param uid the wedding UID
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
              case Some(giftList: models.modules.GiftList) =>
                Ok(views.html.modules.giftList.edit(wedding, giftList, addGiftListItemForm, giftListInfoForm.fill(giftList.moduleContent), giftListInstructionsForm.fill(giftList.instructions)))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle edit form submission
   * @param uid the wedding UID
   */
  def doEdit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
              case Some(giftList: models.modules.GiftList) =>
                giftListInfoForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest(views.html.modules.giftList.edit(wedding, giftList, addGiftListItemForm, formWithErrors, giftListInstructionsForm.fill(giftList.instructions))),
                  giftListInfoNew => {
                    models.modules.GiftList.editGiftList(giftList.copy(moduleContent = giftList.moduleContent.copy(email = giftListInfoNew.email, items = mutable.HashMap())))
                    goToGiftListEdit(uid).flashing("success" -> Messages("main.modules.giftList.modificationsSaved"))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }


  /**
   * Handle edit instructions form submission
   * @param uid the wedding UID
   */

  def doEditInstructions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              giftListInstructionsForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest(views.html.modules.giftList.edit(wedding, giftList, addGiftListItemForm, giftListInfoForm.fill(giftList.moduleContent), formWithErrors)),
                instructions => {
                  models.modules.GiftList.editGiftListInstructions(giftList.copy(instructions = instructions))
                  goToGiftListEdit(uid).flashing("success" -> Messages("main.modules.giftList.modificationsSaved"))
                }
              )
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Display the list of purchases and money transfers for a given wedding
   * @param uid the wedding UID
   */
  def list(uid: String, tab: Int = 0) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
              case Some(giftList: models.modules.GiftList) =>
                val purchaseList = models.modules.GiftList.getAllGiftListPurchases(wedding.id)
                Ok(views.html.modules.giftList.list(wedding, giftList, purchaseList))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Display the PayPal permission request page, waiting for the owner to grant permissions
   * @param uid the wedding UID
   */
  def grantPermissions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              requestPermissions(wedding)
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Callback URL after the owner granted (or not) permissions in PayPal
   * @param uid the wedding UID
   */
  def doGrantPermissions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              val params = parseParameters(request.rawQueryString)
              params.isEmpty match {
                case true =>
                  goToGiftListEdit(uid)
                case false =>
                  val token = params.get("request_token").getOrElse("")
                  val verificationCode = params.get("verification_code").getOrElse("")
                  getAccessToken(wedding, giftList, token, verificationCode)
              }
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Try to verify the permissions of the PayPal account
   * @param uid the wedding UID
   */
  def verifyPermissions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              verifyPayPalInfo(wedding, giftList.moduleContent.paypalInfo)
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Display the PayPal permission request page, waiting for the owner to grant permissions
   * @param uid the wedding UID
   */
  def cancelPermissions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              giftList.moduleContent.paypalInfo.accountId match {
                case Some(payPalAccountId) =>
                  cancelPermissionsPayPal(wedding, giftList.moduleContent.paypalInfo)
                case _ => BadRequest
              }
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX request for the PayPal balance
   * @param uid the wedding UID
   */
  def getBalance(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              getBalancePayPal(wedding, giftList.moduleContent.paypalInfo)
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX request for testing if it is possible to make a payment with PayPal
   * @param uid the wedding UID
   */
  def checkPayPal(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) =>
              testExpressCheckout(giftList, wedding)
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX gift addition form submission
   * @param uid the wedding UID
   */
  def addItem(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
              case Some(giftList: models.modules.GiftList) =>
                addGiftListItemForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest,
                  giftListItem => {
                    Ok(views.html.modules.giftList.editItem(models.modules.GiftList.addGiftListItem(giftListItem.copy(weddingId = wedding.id)), giftList.moduleContent.currency))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX gift edition form submission
   * @param uid the wedding UID
   */
  def editItem(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
              case Some(giftList: models.modules.GiftList) =>
                editGiftListItemForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest,
                  giftListItem => {
                    giftList.moduleContent.items.get(giftListItem.id.get) match {
                      case Some(_) =>
                        Ok(views.html.modules.giftList.editItem(models.modules.GiftList.editGiftListItem(giftListItem.copy(weddingId = wedding.id)), giftList.moduleContent.currency))
                      case _ =>
                        NotFound
                    }
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX gift deletion
   * @param uid the wedding UID
   * @param itemId the ID of the delted item
   */
  def deleteItem(uid: String, itemId: Long) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
              case Some(giftList: models.modules.GiftList) =>
                giftList.moduleContent.items.get(itemId) match {
                  case Some(_) => {
                    models.modules.GiftList.inactiveGiftListItem(Id(itemId))
                    Ok
                  }
                  case _ =>
                    NotFound
                }
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Initialize the PayPal Express Checkout and when done redirect the user to PayPal
   * @param cart the cart
   * @param giftList the gift list
   * @param wedding the wedding
   */
  def setExpressCheckout(cart: models.modules.Cart, giftList: models.modules.GiftList, wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader) = {
    val params = mutable.HashMap[String, String]()
    val method = "SetExpressCheckout"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val style = play.Play.application().configuration().getString("paypal.style")
    val returnUrl = controllers.modules.routes.GiftList.checkout(wedding.uid).absoluteURL(isSecureConnection)
    val cancelUrl = controllers.modules.routes.GiftList.display(wedding.uid).absoluteURL(isSecureConnection)
    val totalCost = cart.items.values.map(_.total).sum
    val total = totalCost.toString
    val currency = giftList.moduleContent.currency
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")
    val subject = giftList.moduleContent.paypalInfo.accountId.getOrElse("")

    // Add login infos
    params.put("METHOD", method)
    params.put("VERSION", version)
    params.put("USER", username)
    params.put("PWD", password)
    params.put("SIGNATURE", signature)
//    params.put("EMAIL", email) // Warning, this is to pre-fill the buyer's mail address
    params.put("RETURNURL", returnUrl)
    params.put("CANCELURL", cancelUrl)
    params.put("LOCALECODE", lang.country)
    params.put("NOSHIPPING", "1")
    params.put("SOLUTIONTYPE", "Sole") // No paypal account needed
    params.put("LANDINGPAGE", "Billing") // Non login page
    params.put("CHANNELTYPE", "Merchant")
    params.put("PAGESTYLE", style)
    params.put("PAYMENTREQUEST_0_AMT", total)
    params.put("PAYMENTREQUEST_0_CURRENCYCODE", currency)
    params.put("PAYMENTREQUEST_0_CUSTOM", Messages("main.modules.giftList.giftList") + " " + Messages("main.wedding.wedding") + " " + wedding.uid)
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("GIFTMESSAGEENABLE", "0")
    params.put("GIFTRECEIPTENABLE", "0")
    params.put("GIFTWRAPENABLE", "0")
    params.put("SUBJECT", subject)
    var i = 0
    for ((id, cartItem) <- cart.items) {
      params.put("L_PAYMENTREQUEST_0_NAME"+i, cartItem.item.description)
      params.put("L_PAYMENTREQUEST_0_AMT"+i, cartItem.item.unitPrice.toString)
      params.put("L_PAYMENTREQUEST_0_QTY"+i, cartItem.number.toString)
      i += 1
    }

    Async {
      WS.url(apiUrl).post(formatRequest(params)).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("ACK") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val token = result.get("TOKEN").getOrElse("");
          val correlationId = result.get("CORRELATIONID").getOrElse("");
          val giftListPurchase = models.modules.GiftListPurchase(null, wedding.id, null, null, None, None, Some(token), None, Some(correlationId), None, None, cart, currency, totalCost, computeAllFees(totalCost, currency))
          Cache.set(PURCHASE_CACHE_KEY, giftListPurchase, 15*60)
          Redirect(play.Play.application().configuration().getString("paypal.httpUrl") + "/webscr?cmd=_express-checkout&token=" + token)
        } else {
          Logger.error("SetExpressCheckout for gift list has failed");
          getAndLogPayPalError(result)
          goToGiftListDisplay(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.setExpressCheckout"))
        }
      }
    }
  }

  /**
   * Send the DoExpressCheckout request to PayPal, notify the owner and the buyer if everything is OK and display the invoice
   * @param wedding the wedding
   * @param purchase the current purchase
   */
  def doExpressCheckout(wedding: models.wedding.Wedding, purchase: models.modules.GiftListPurchase, giftList: models.modules.GiftList)(implicit request: play.api.mvc.RequestHeader) = {
    val params = mutable.HashMap[String, String]()
    val method = "DoExpressCheckoutPayment"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val style = play.Play.application().configuration().getString("paypal.style")
    val totalCost = purchase.totalCost
    val total = totalCost.toString
    val currencyStr = purchase.currency
    val payerId = purchase.payerId
    val token = purchase.token
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")
    val subject = giftList.moduleContent.paypalInfo.accountId.getOrElse("")

    // Add login infos
    params.put("METHOD", method)
    params.put("VERSION", version)
    params.put("USER", username)
    params.put("PWD", password)
    params.put("SIGNATURE", signature)
//    params.put("EMAIL", email) // Warning, this is to pre-fill the buyer's mail address
    params.put("PAYERID", payerId.getOrElse(""))
    params.put("TOKEN", token.getOrElse(""))
    params.put("LOCALECODE", lang.country)
    params.put("PAGESTYLE", style)
    params.put("PAYMENTREQUEST_0_AMT", total)
    params.put("PAYMENTREQUEST_0_CURRENCYCODE", currencyStr)
    params.put("PAYMENTREQUEST_0_CUSTOM", Messages("main.modules.giftList.giftList") + " " + Messages("main.wedding.wedding") + " " + wedding.uid)
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("SUBJECT", subject)
    var i = 0
    for ((id, cartItem) <- purchase.cart.items) {
      params.put("L_PAYMENTREQUEST_0_NAME"+i, cartItem.item.description)
      params.put("L_PAYMENTREQUEST_0_AMT"+i, cartItem.item.unitPrice.toString)
      params.put("L_PAYMENTREQUEST_0_QTY"+i, cartItem.number.toString)
      i += 1
    }

    Async {
      WS.url(apiUrl).post(formatRequest(params)).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("ACK") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val completePurchase = purchase.copy(transactionId = result.get("PAYMENTINFO_0_TRANSACTIONID"), accountId = result.get("PAYMENTINFO_0_SECUREMERCHANTACCOUNTID"), date = new java.util.Date())
          val finalPurchase = models.modules.GiftList.addGiftListPurchase(wedding, completePurchase)
          Cache.set(PURCHASE_CACHE_KEY, null, 0)
          Cache.set(getCartCacheKey(wedding), null, 0)
          wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
            case Some(giftList: models.modules.GiftList) => {
              if (giftList.moduleContent.email.isDefined) {
                try {
                  val view = views.html.modules.giftList.mailNotification(finalPurchase, wedding, controllers.modules.routes.GiftList.list(wedding.uid).absoluteURL(isSecureConnection))
                  val message = view.body
                  NotificationCenter.notificationMail(giftList.moduleContent.email.get, "[INFO] "+Messages("main.modules.giftList.paymentReceived"), message)
                } catch {
                  case error => {
                    Logger.error("[ERR] Could not send gift list notification mail to "+giftList.moduleContent.email.getOrElse("")+".")
                    Logger.error("[ERR] "+error.toString)
                  }
                }
              }
            }
            case _ =>
          }
          if (finalPurchase.email.isDefined) {
            try {
              val view = views.html.modules.giftList.mailInvoice(finalPurchase, giftList.moduleContent.paypalInfo, wedding)
              val message: String = view.body
              NotificationCenter.notificationMail(finalPurchase.email.get, Messages("main.modules.giftList.paymentConfirmation"), message)
            } catch {
              case error => {
                Logger.error("[ERR] Could not send gift list confirmation mail to "+finalPurchase.email.getOrElse("")+".")
                Logger.error("[ERR] "+error.toString)
              }
            }
          }
          Ok(views.html.modules.giftList.thankYou(wedding, finalPurchase))
        } else {
          Logger.error("DoExpressCheckout gift list has failed");
          getAndLogPayPalError(result)
          goToGiftListDisplay(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.doExpressCheckout"))
        }
      }
    }
  }

  /**
   * Test the PayPal Express Checkout with permissions
   * @param giftList the gift list
   * @param wedding the wedding
   */
  def testExpressCheckout(giftList: models.modules.GiftList, wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader) = {
    val params = mutable.HashMap[String, String]()
    val method = "SetExpressCheckout"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val returnUrl = controllers.modules.routes.GiftList.edit(wedding.uid).absoluteURL(isSecureConnection)
    val cancelUrl = controllers.modules.routes.GiftList.edit(wedding.uid).absoluteURL(isSecureConnection)
    val totalCost = 0.01
    val total = totalCost.toString
    val currency = giftList.moduleContent.currency
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")
    val subject = giftList.moduleContent.paypalInfo.accountId.getOrElse("")

    // Add login infos
    params.put("METHOD", method)
    params.put("VERSION", version)
    params.put("USER", username)
    params.put("PWD", password)
    params.put("SIGNATURE", signature)
//    params.put("EMAIL", email) // Warning, this is to pre-fill the buyer's mail address
    params.put("RETURNURL", returnUrl)
    params.put("CANCELURL", cancelUrl)
    params.put("LOCALECODE", lang.country)
    params.put("NOSHIPPING", "1")
    params.put("SOLUTIONTYPE", "Sole") // No paypal account needed
    params.put("LANDINGPAGE", "Billing") // Non login page
    params.put("CHANNELTYPE", "Merchant")
    params.put("PAYMENTREQUEST_0_AMT", total)
    params.put("PAYMENTREQUEST_0_CURRENCYCODE", currency)
    params.put("PAYMENTREQUEST_0_CUSTOM", Messages("main.modules.giftList.testExpressCheckout"))
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("GIFTMESSAGEENABLE", "0")
    params.put("GIFTRECEIPTENABLE", "0")
    params.put("GIFTWRAPENABLE", "0")
    params.put("SUBJECT", subject)

    Async {
      WS.url(apiUrl).post(formatRequest(params)).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("ACK") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          Ok
        } else {
          Logger.error("Test SetExpressCheckout for gift list has failed");
          getAndLogPayPalError(result)
          val reason = result.get("L_ERRORCODE0").getOrElse("") match {
            case "10425" => Messages("main.modules.giftList.paypal.error.expressCheckoutDisabled")
            case _ => Messages("main.modules.giftList.paypal.error.unknownError")
          }
          InternalServerError(reason)
        }
      }
    }
  }

  /**
   * Send a request to PayPal to ask for permissions on an account and redirect the user to the PayPal login
   * in order to make him/her confirm the permission
   * @param wedding the wedding
   */
  def requestPermissions(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader) = {
    val method = "RequestPermissions"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val applicationId = play.Play.application().configuration().getString("paypal.applicationId")
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrlPermissions")+method
    val callbackUrl = controllers.modules.routes.GiftList.doGrantPermissions(wedding.uid).absoluteURL(isSecureConnection)
    val params = Map(
      "version" -> Seq(version),
      "email" -> Seq(email),
      "localecode" -> Seq(lang.country),
      "callback" -> Seq(callbackUrl),
      "scope(0)" -> Seq("ACCESS_BASIC_PERSONAL_DATA"),
      "scope(1)" -> Seq("EXPRESS_CHECKOUT")
      //"scope(2)" -> Seq("ACCOUNT_BALANCE") // PayPal bug : only 2 scopes possible!
    )

    val header1 = ("X-PAYPAL-REQUEST-DATA-FORMAT", "NV")
    val header2 = ("X-PAYPAL-RESPONSE-DATA-FORMAT", "NV")
    val header3 = ("X-PAYPAL-SECURITY-USERID", username)
    val header4 = ("X-PAYPAL-SECURITY-PASSWORD", password)
    val header5 = ("X-PAYPAL-SECURITY-SIGNATURE", signature)
    val header6 = ("X-PAYPAL-APPLICATION-ID", applicationId)

    Async {
      WS.url(apiUrl).withHeaders(header1, header2, header3, header4, header5, header6).post(params).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("responseEnvelope.ack") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val token = result.get("token").getOrElse("");
          Redirect(play.Play.application().configuration().getString("paypal.httpUrl") + "/webscr?cmd=_grant-permission&request_token=" + token)
        } else {
          Logger.error("RequestPermissions for gift list has failed");
          getAndLogPayPalErrorEnvelope(result)
          goToGiftListEdit(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.requestPermissions"))
        }
      }
    }
  }

  /**
   * After the user has granted the permissions, save the sent infos that will then be used for authorization with third party
   * @param wedding the wedding
   * @param giftList the loaded gift list
   * @param token the PayPal request token
   * @param verificationCode the PayPal verification code
   */
  def getAccessToken(wedding: models.wedding.Wedding, giftList: models.modules.GiftList, token: String, verificationCode: String)(implicit request: play.api.mvc.RequestHeader) = {
    val method = "GetAccessToken"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val applicationId = play.Play.application().configuration().getString("paypal.applicationId")
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrlPermissions")+method

    val params = Map(
      "version" -> Seq(version),
      "email" -> Seq(email),
      "localecode" -> Seq(lang.country),
      "token" -> Seq(token),
      "verifier" -> Seq(verificationCode)
    )

    val header1 = ("X-PAYPAL-REQUEST-DATA-FORMAT", "NV")
    val header2 = ("X-PAYPAL-RESPONSE-DATA-FORMAT", "NV")
    val header3 = ("X-PAYPAL-SECURITY-USERID", username)
    val header4 = ("X-PAYPAL-SECURITY-PASSWORD", password)
    val header5 = ("X-PAYPAL-SECURITY-SIGNATURE", signature)
    val header6 = ("X-PAYPAL-APPLICATION-ID", applicationId)

    Async {
      WS.url(apiUrl).withHeaders(header1, header2, header3, header4, header5, header6).post(params).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("responseEnvelope.ack") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val accessToken = result.get("token").getOrElse("");
          val secret = result.get("tokenSecret").getOrElse("");
          val payPalInfo = giftList.moduleContent.paypalInfo.copy(status = models.modules.GiftList.PAYPAL_PERMISSIONS_STATUS_NOT_VERIFIED, statusDate = new Date(), token = Some(accessToken), secret = Some(secret))
          models.modules.GiftList.editGiftListPayPalInfo(wedding.id, payPalInfo)
          Redirect(controllers.modules.routes.GiftList.verifyPermissions(wedding.uid))
        } else {
          Logger.error("GetAccessToken for gift list has failed");
          getAndLogPayPalErrorEnvelope(result)
          goToGiftListEdit(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.getAccessToken"))
        }
      }
    }
  }

  /**
   * Verify if the permissions are correctly set and get the PayPal account email / ID for further use
   * @param wedding the wedding
   * @param paypalInfo the PayPal info
   */
  def verifyPayPalInfo(wedding: models.wedding.Wedding, paypalInfo: GiftListPayPalInfo) = {
    import com.paypal.sdk.util.OAuthSignature
    val method = "GetBasicPersonalData"
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val applicationId = play.Play.application().configuration().getString("paypal.applicationId")
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrlPermissions")+method
    val httpMethod = OAuthSignature.HTTPMethod.POST

    val PP_PERSONAL_DATA_EMAIL_URI = "http://axschema.org/contact/email"
    val PP_PERSONAL_DATA_FULLNAME_URI = "http://schema.openid.net/contact/fullname"

    val params = Map(
      "attributeList.attribute(0)" -> Seq(PP_PERSONAL_DATA_EMAIL_URI),
      "attributeList.attribute(1)" -> Seq(PP_PERSONAL_DATA_FULLNAME_URI)
    )

    val header1 = ("X-PAYPAL-REQUEST-DATA-FORMAT", "NV")
    val header2 = ("X-PAYPAL-RESPONSE-DATA-FORMAT", "NV")
    val header3 = ("X-PAYPAL-SECURITY-USERID", username)
    val header4 = ("X-PAYPAL-SECURITY-PASSWORD", password)
    val header5 = ("X-PAYPAL-SECURITY-SIGNATURE", signature)
    val header6 = ("X-PAYPAL-APPLICATION-ID", applicationId)
    val header7 = ("X-PAYPAL-AUTHORIZATION", OAuthSignature.getFullAuthString(username, password, paypalInfo.token.getOrElse(""), paypalInfo.secret.getOrElse(""), httpMethod, apiUrl, null))

    Async {
      WS.url(apiUrl).withHeaders(header1, header2, header3, header4, header5, header6, header7).post(params).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("responseEnvelope.ack") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val accountEmail = result.get("response.personalData(0).personalDataValue")
          val fullName = result.get("response.personalData(1).personalDataValue")
          accountEmail match {
            case Some(email) => {
              val paypalInfoFull = paypalInfo.copy(accountId = accountEmail, status = models.modules.GiftList.PAYPAL_PERMISSIONS_STATUS_VERIFIED, statusDate = new Date(), fullName = fullName)
              models.modules.GiftList.editGiftListPayPalInfo(wedding.id, paypalInfoFull)
              goToGiftListEdit(wedding.uid).flashing("success" -> Messages("main.modules.giftList.paypal.success.grantPermissions"))
            }
            case _ => {
              Logger.error("GetBasicPersonalData for gift list has failed (no account ID received)");
              getAndLogPayPalErrorEnvelope(result)
              goToGiftListEdit(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.getBasicPersonalData"))
            }
          }
        } else {
          Logger.error("GetBasicPersonalData for gift list has failed");
          getAndLogPayPalErrorEnvelope(result)
          goToGiftListEdit(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.getBasicPersonalData"))
        }
      }
    }
  }

  /**
   * Cancel the PayPal permissions
   * @param wedding the wedding
   * @param paypalInfo the PayPal info
   */
  def cancelPermissionsPayPal(wedding: models.wedding.Wedding, paypalInfo: GiftListPayPalInfo) = {
    val method = "CancelPermissions"
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val applicationId = play.Play.application().configuration().getString("paypal.applicationId")
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrlPermissions")+method

    val params = Map(
      "token" -> Seq(paypalInfo.token.getOrElse(""))
    )

    val header1 = ("X-PAYPAL-REQUEST-DATA-FORMAT", "NV")
    val header2 = ("X-PAYPAL-RESPONSE-DATA-FORMAT", "NV")
    val header3 = ("X-PAYPAL-SECURITY-USERID", username)
    val header4 = ("X-PAYPAL-SECURITY-PASSWORD", password)
    val header5 = ("X-PAYPAL-SECURITY-SIGNATURE", signature)
    val header6 = ("X-PAYPAL-APPLICATION-ID", applicationId)

    Async {
      WS.url(apiUrl).withHeaders(header1, header2, header3, header4, header5, header6).post(params).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("responseEnvelope.ack") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val paypalInfoReset = paypalInfo.copy(accountId = None, status = models.modules.GiftList.PAYPAL_PERMISSIONS_STATUS_NOT_WORKING, statusDate = new Date(), token = None, secret = None, fullName = None)
          models.modules.GiftList.editGiftListPayPalInfo(wedding.id, paypalInfoReset)
          goToGiftListEdit(wedding.uid).flashing("success" -> Messages("main.modules.giftList.paypal.success.cancelPermissions"))
        } else {
          Logger.error("CancelPermissions for gift list has failed");
          getAndLogPayPalErrorEnvelope(result)
          goToGiftListEdit(wedding.uid).flashing("error" -> Messages("main.modules.giftList.paypal.error.cancelPermissions"))
        }
      }
    }
  }

  /**
   * Get the PayPal account balance for the given wedding
   * @param wedding the wedding
   * @param paypalInfo the PayPal info
   */
  def getBalancePayPal(wedding: models.wedding.Wedding, paypalInfo: GiftListPayPalInfo)(implicit request: play.api.mvc.RequestHeader) = {
    val params = mutable.HashMap[String, String]()
    val method = "GetBalance"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")
    val subject = paypalInfo.accountId.getOrElse("")

    // Add login infos
    params.put("METHOD", method)
    params.put("VERSION", version)
    params.put("USER", username)
    params.put("PWD", password)
    params.put("SIGNATURE", signature)
    params.put("EMAIL", email)
    params.put("LOCALECODE", lang.country)
    params.put("SUBJECT", subject)

    Async {
      WS.url(apiUrl).post(formatRequest(params)).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("ACK") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val amount = result.get("L_AMT0").getOrElse("0").toDouble
          val currency = result.get("L_CURRENCYCODE0").getOrElse("")
          Ok(toJson(Map(
            "currency" -> currency,
            "amount" -> views.html.helper.currency(amount).toString
          )))
        } else {
          val error = result.get("L_SHORTMESSAGE0")
          val errorExplanation = result.get("L_LONGMESSAGE0")
          Logger.error("Paypal account balance denied: "+error.getOrElse("")+" ("+errorExplanation.getOrElse("")+")")
          InternalServerError
        }
      }
    }
  }

  /**
   * Compute all the fees for the given transaction amount
   * @param amount the transaction amount
   * @param currency the transaction currency
   * @return the fees amount to be deduced
   */
  def computeAllFees(amount: Double, currency: String): Double = {
    /**
     * For the moment, 3.4% + 0.55 CHF (PayPal)
     */
    amount * 0.034 + 0.55
  }

  /**
   * Redirect to the edit form of the gift list
   */
  def goToGiftListEdit(uid: String) = Redirect(controllers.modules.routes.GiftList.edit(uid))

  /**
   * Redirect to the gift list display
   */
  def goToGiftListDisplay(uid: String) = Redirect(controllers.modules.routes.GiftList.display(uid))

  /**
   * Return the carte cache key for the given wedding (avoid to share the same cart for different weddings!)
   * @param wedding
   */
  protected def getCartCacheKey(wedding: models.wedding.Wedding): String = {
    CART_CACHE_KEY+"_"+wedding.id.getOrElse(0).toString
  }
}
