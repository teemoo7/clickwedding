package controllers.providers

import controllers.AnyController
import play.api.data.Form
import play.api.data.Forms._
import anorm.{Id, Pk, NotAssigned}
import play.api.data.format.Formats._
import java.util.Date
import play.api.i18n.{Lang, Messages}
import play.api.mvc.Action
import models.authentication.User
import controllers.admin.NotificationCenter
import collection.mutable
import play.api.libs.ws.WS
import controllers.helpers.WSHelper._
import scala.{None, Some}
import anorm.Id
import play.api.cache.Cache
import play.api.{Routes, Logger}
import models.providers.{ProviderStatsEvent, ProviderTransaction}
import controllers.helpers.UtilsHelper
import play.api.Play.current
import concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.libs.json.Json._
import anorm.Id
import java.io.File
import app.controllers.helpers.ImagesHelper

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.09.13
 * Time: 14:39
 * To change this template use File | Settings | File Templates.
 */
object Provider extends AnyController {

  val COUNTRIES = Seq("CH", "FR", "DE", "IT")

  val PURCHASE_CACHE_KEY = "PayPalExpressCheckoutSPPurchase"

  /**
   * New provider form definition
   */
  val addProviderForm: Form[models.providers.Provider] = Form(
    mapping(
      "title" -> nonEmptyText(maxLength = 50),
      "slogan" -> optional(text(minLength = 0, maxLength = 100)),
      "name" -> nonEmptyText(maxLength = 100),
      "street" -> nonEmptyText(maxLength = 100),
      "streetNb" -> optional(text(minLength = 0, maxLength = 8)),
      "place" -> nonEmptyText(maxLength = 50),
      "zip" -> nonEmptyText(maxLength = 10),
      "country" -> nonEmptyText(maxLength = 50),
      "phone" -> optional(text(minLength = 0, maxLength = 20)),
      "email" -> nonEmptyEmail,
      "website" -> optional(text(minLength = 0, maxLength = 100)),
      "description" -> optional(text(minLength = 0, maxLength = 300)),
      "latitude" -> of[Double],
      "longitude" -> of[Double],
      "categoryId" -> number,
      "acceptTerms" -> checked(Messages("main.wedding.pleaseAcceptTermsAndConditions")),
      "acceptProvider" -> checked(Messages("main.wedding.pleaseAcceptTermsAndConditions")),
      "packId" -> number.verifying(Messages("main.providers.pleaseChooseAPack"), packId => models.providers.Provider.getAvailablePackTypes.map(_.id.get.toInt).contains(packId))
  )
      ((title, slogan, name, street, streetNb, place, zip, country, phone, email, website, description, latitude, longitude, categoryId, acceptTerms, acceptProvider, packId) => models.providers.Provider(null, title, slogan, name, street, streetNb, place, zip, country, phone, email, website, None, List(), description, latitude, longitude, categoryId, false, new Date(), models.providers.Provider.getPackForNewPurchase(Id(packId.toLong))))
      ((provider: models.providers.Provider) => Some(provider.title, provider.slogan, provider.name, provider.street, provider.streetNb, provider.place, provider.zip, provider.country, provider.phone, provider.email, provider.website, provider.description, provider.latitude, provider.longitude, provider.categoryId.toInt, false, false, -1))
  )

  /**
   * Provider edition form definition
   */
  val editProviderForm: Form[models.providers.Provider] = Form(
    mapping(
      "id" -> number,
      "title" -> nonEmptyText(maxLength = 50),
      "slogan" -> optional(text(minLength = 0, maxLength = 100)),
      "name" -> nonEmptyText(maxLength = 100),
      "street" -> nonEmptyText(maxLength = 100),
      "streetNb" -> optional(text(minLength = 0, maxLength = 8)),
      "place" -> nonEmptyText(maxLength = 50),
      "zip" -> nonEmptyText(maxLength = 10),
      "country" -> nonEmptyText(maxLength = 50),
      "phone" -> optional(text(minLength = 0, maxLength = 20)),
      "email" -> nonEmptyEmail,
      "website" -> optional(text(minLength = 0, maxLength = 100)),
      "description" -> optional(text(minLength = 0, maxLength = 300)),
      "latitude" -> of[Double],
      "longitude" -> of[Double],
      "categoryId" -> number
    )
      ((id, title, slogan, name, street, streetNb, place, zip, country, phone, email, website, description, latitude, longitude, categoryId) => models.providers.Provider(Id(id.toLong), title, slogan, name, street, streetNb, place, zip, country, phone, email, website, None, List(), description, latitude, longitude, categoryId, false, new Date(), None))
      ((provider: models.providers.Provider) => Some(provider.id.get.toInt, provider.title, provider.slogan, provider.name, provider.street, provider.streetNb, provider.place, provider.zip, provider.country, provider.phone, provider.email, provider.website, provider.description, provider.latitude, provider.longitude, provider.categoryId.toInt))
  )

  /**
   * Contact form definition
   */
  val contactForm = Form(
    tuple(
      "name" -> nonEmptyText,
      "email" -> nonEmptyEmail,
      "title" -> nonEmptyText,
      "message" -> nonEmptyText,
      "captchaUuid" -> nonEmptyText,
      "captchaValue" -> nonEmptyText
    ).verifying(Messages("main.captcha.captchaDoesNotMatch"), form => form match {
      case (name, email, title, message, captchaUuid, captchaValue) => validateCaptcha(captchaValue, captchaUuid)
    })
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Provider.detail,
        routes.javascript.Provider.website,
        routes.javascript.Provider.contactMail,
        routes.javascript.Provider.search,
        routes.javascript.Provider.addLogo,
        routes.javascript.Provider.deleteLogo,
        routes.javascript.Provider.addPicture,
        routes.javascript.Provider.deletePicture
    )
    ).as("text/javascript")
  }

  /**
   * Provider list redirection
   */
  def gotoList =
    Redirect(controllers.providers.routes.Provider.list)

  /**
   * Provider details redirection
   */
  def gotoDetail(id: Long) =
    Redirect(controllers.providers.routes.Provider.detail(id))

  /**
   * Provider edition redirection
   */
  def gotoEdition(id: Long) =
    Redirect(controllers.providers.routes.Provider.edit(id))

  /**
   * Purchase invoice redirection
   */
  def gotoInvoice(purchaseId: Long) =
    Redirect(controllers.providers.routes.Provider.invoice(purchaseId))

  /**
   * Display the list of service providers with search form
   */
  def list = UserAwareAction {
    implicit request =>
      val userProviders = request.user match {
        case Some(identity) => models.providers.Provider.getProvidersByUserId(identity.asInstanceOf[User])
        case None => List()
      }
      Ok(views.html.providers.list(userProviders))
  }

  /**
   * Ajax call to search all providers in a given category
   * @param categoryId
   * @return a list of providers
   */
  def search(categoryId: Long) = Action {
    implicit request =>
      val providers = models.providers.Provider.getAllProvidersForCategory(categoryId)
      providers.map(p => models.providers.Provider.insertProviderStatsEvent(new ProviderStatsEvent(p.id, new Date(), request.remoteAddress, models.providers.Provider.STATS_EVENT_TYPE_DISPLAY)))
      Ok(toJson(providers))
  }

  /**
   * Display the detailed info for the given provider
   * @param id the provider ID
   */
  def detail(id: Long) = Action {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) =>
          models.providers.Provider.insertProviderStatsEvent(new ProviderStatsEvent(provider.id, new Date(), request.remoteAddress, models.providers.Provider.STATS_EVENT_TYPE_DETAIL))
          Ok(views.html.providers.detail(provider))
        case None =>
          NotFound
      }
  }

  /**
   * Redirect to the website of the given provider
   * @param id the provider ID
   */
  def website(id: Long) = Action {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) =>
          provider.website match {
            case Some(website) => {
              models.providers.Provider.insertProviderStatsEvent(new ProviderStatsEvent(provider.id, new Date(), request.remoteAddress, models.providers.Provider.STATS_EVENT_TYPE_WEBSITE))
              Redirect(provider.website.get)
            }
            case None =>
              NotFound
          }
        case None =>
          NotFound
      }
  }

  /**
   * Display the email form to contact a provider
   * @param id the provider ID
   */
  def contactMail(id: Long) = Action {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) =>
          Ok(views.html.providers.contact(contactForm, provider))
        case None =>
          NotFound
      }
  }

  /**
   * Handle email form submission to contact a provider
   * @param id the provider ID
   */
  def doContactMail(id: Long) = Action {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) =>
          contactForm.bindFromRequest.fold(
            formWithErrors =>
              BadRequest(views.html.providers.contact(formWithErrors, provider)),
            contactFormOk => {
              val toAddress = provider.email
              val fromAddressAntiSpam = contactFormOk._1+" via click-wedding <"+play.Play.application().configuration().getString("notificationMail")+">"
              val fromAddress = contactFormOk._1+" <"+contactFormOk._2+">"
              val subject = contactFormOk._3
              val message = views.html.admin.notification.providers.mailContact(contactFormOk._1, contactFormOk._2, contactFormOk._3, contactFormOk._4).body
              try {
                controllers.admin.NotificationCenter.sendMail(toAddress, fromAddressAntiSpam, subject, message, false, Some(fromAddress))
                models.providers.Provider.insertProviderStatsEvent(new ProviderStatsEvent(provider.id, new Date(), request.remoteAddress, models.providers.Provider.STATS_EVENT_TYPE_MAIL))
                gotoDetail(id).flashing("success" -> Messages("main.providers.contact.successMailSent"))
              } catch {
                case error => {
                  BadRequest(views.html.providers.contact(contactForm.fill(contactFormOk).withGlobalError(Messages("main.providers.contact.errorMailNotSent")), provider))
                }
              }
            }
          )
        case None =>
          NotFound
      }
  }

  /**
   * Render a PDF file to be printed with the provider info
   * @param id the provider ID
   */
  def print(id: Long) = Action {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) =>
          Ok(views.html.providers.print(provider))
        case None =>
          NotFound
      }
  }

  /**
   * Display an information page to become service provider
   */
  def info = Action {
    implicit request =>
      Ok(views.html.providers.info(models.providers.Provider.getAvailablePackTypes))
  }

  /**
   * Display a form to add a new provider
   */
  def create = SecuredAction {
    implicit request =>
      Ok(views.html.providers.create(addProviderForm, models.providers.Provider.getAvailablePackTypes))
  }

  /**
   * Handle new provider registration form
   */
  def doCreate = SecuredAction {
    implicit request =>
      addProviderForm.bindFromRequest.fold(
        formWithErrors =>
          BadRequest(views.html.providers.create(formWithErrors, models.providers.Provider.getAvailablePackTypes)),
        provider => {
          val providerCreated = models.providers.Provider.addProvider(provider, request.user.asInstanceOf[User])
          // If pack is free, then update its info to be set as paid
          if (providerCreated.currentPack.get.purchase.amount == 0) {
            models.providers.Provider.updatePurchasePayment(
              providerCreated.currentPack.get.purchase.copy(
                paid = true,
                paymentDate = Some(providerCreated.currentPack.get.purchase.since),
                paymentMethod = Some(models.providers.Provider.PAYMENT_METHOD_FREE)
              )
            )
          }
          val view = views.html.admin.notification.providers.mailNotificationProviderCreated(providerCreated)
          val message = view.body
          NotificationCenter.adminNotificationMail("[ACTION] Nouveau prestataire", message)
          gotoList.flashing("success" -> Messages("main.providers.successSubscriptionSaved"))
        }
      )
  }

  /**
   * Display the edition form (not admin)
   * @param id the service provider ID
   */
  def edit(id: Long) = SecuredAction(IsOwnerOfServiceProvider(id)) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) =>
          Ok(views.html.providers.edit(editProviderForm.fill(provider), provider))
        case None =>
          NotFound
      }
  }

  /**
   * Handle edition form (not admin)
   * @param id the service provider ID
   */
  def doEdit(id: Long) = SecuredAction(IsOwnerOfServiceProvider(id)) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(currentProvider) => {
          editProviderForm.bindFromRequest.fold(
            formWithErrors =>
              BadRequest(views.html.providers.edit(formWithErrors, currentProvider)),
            provider => {
              models.providers.Provider.editProvider(provider)
              gotoList.flashing("success" -> Messages("main.providers.successSubscriptionEdited"))
            }
          )
        }
        case None =>
          NotFound
      }
  }

  /**
   * Handle AJAX logo picture upload for given provider
   * @param id the provider ID
   */
  def addLogo(id:Long) = SecuredAction(true, IsOwnerOfServiceProvider(id)) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(currentProvider) => {
          request.body.asMultipartFormData match {
            case Some(body) => {
              val responses: Seq[play.api.templates.Html] = body.files.map {
                uploadFilePart =>
                  val contentType = uploadFilePart.contentType.getOrElse("image/jpeg")
                  val file = uploadFilePart.ref
                  val logoPicture = models.providers.ProviderLogoPicture(id, Some(uploadFilePart.filename), Some(contentType))
                  // The picture should already be resized with JS but this may not work with old browsers. So resize it here again (and compress it a bit more)
                  val resizedFile = getResizedLogoPicture(logoPicture, file.file)
                  models.providers.Provider.addLogo(logoPicture, resizedFile)
                  views.html.providers.logoPictureEdit(logoPicture)
              }
              Ok(mkHtml(responses.toList))
            }
            case _ => {
              BadRequest
            }
          }
        }
        case None =>
          NotFound
      }
  }

  /**
   * Handle logo deletion AJAX call for given provider
   * @param id the provider ID
   */
  def deleteLogo(id:Long) = SecuredAction(true, IsOwnerOfServiceProvider(id)) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(currentProvider) => {
          val logoPicture = models.providers.ProviderLogoPicture(id, None, None)
          models.providers.Provider.deleteLogo(logoPicture)
          Ok
        }
      case None =>
        NotFound
    }
  }

  /**
   * Handle AJAX picture upload for given provider
   * @param id the provider ID
   */
  def addPicture(id:Long) = SecuredAction(true, IsOwnerOfServiceProvider(id)) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(currentProvider) => {
          if (currentProvider.isShowroomQuotaReached) {
            BadRequest(Messages("main.providers.error.quotaExceeded"))
          } else {
            request.body.asMultipartFormData match {
              case Some(body) => {
                val responses: Seq[play.api.templates.Html] = body.files.map {
                  uploadFilePart =>
                    val contentType = uploadFilePart.contentType.getOrElse("image/jpeg")
                    val file = uploadFilePart.ref
                    val picture = models.providers.ProviderShowroomPicture(null, id, Some(uploadFilePart.filename), file.file.length.toInt, new Date(), Some(contentType), None)
                    // The picture should already be resized with JS but this may not work with old browsers. So resize it here again (and compress it a bit more)
                    val resizedPicture = getResizedShowroomPicture(picture, file.file)
                    val thumbnail = getThumbnailShowroomPicture(picture, resizedPicture)
                    val pictureWithId = models.providers.Provider.addPicture(picture.copy(size = resizedPicture.length.toInt), resizedPicture, thumbnail)
                    views.html.providers.showroomPictureEdit(pictureWithId)
                }
                Ok(mkHtml(responses.toList))
              }
              case _ => {
                BadRequest
              }
            }
          }
        }
        case None =>
          NotFound
      }
  }

  /**
   * Handle picture deletion AJAX call for given provider
   * @param id the provider ID
   * @param pid the picture ID
   */
  def deletePicture(id:Long, pid: Long) = SecuredAction(true, IsOwnerOfServiceProvider(id)) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(currentProvider) => {
          currentProvider.showroom.find(p => p.id.get == pid) match {
            case Some(showroomPicture) => {
              models.providers.Provider.deletePicture(showroomPicture)
              Ok
            }
            case None =>
              NotFound
          }
        }
      case None =>
        NotFound
    }
  }

  /**
   * Display the purchase info for the given invoice, with print and PDF export functions. Explanations for payment
   * possibilities
   * @param purchaseId the purchase ID
   */
  def invoice(purchaseId: Long) = SecuredAction(IsOwnerOfServiceProviderPurchase(purchaseId)) {
    implicit request =>
      models.providers.Provider.getProviderForPurchase(Id(purchaseId)) match {
        case Some(provider) => {
          val transaction = models.providers.Provider.getTransaction(purchaseId)
          Ok(views.html.providers.invoice(provider, provider.currentPack.get.purchase, provider.currentPack.get.packType, transaction))
        }
        case None =>
          NotFound
      }
  }

  /**
   * Render the purchase info for the given invoice as a PDF file
   * @param purchaseId the purchase ID
   */
  def invoicePDF(purchaseId: Long) = SecuredAction(IsOwnerOfServiceProviderPurchase(purchaseId)) {
    implicit request =>
      models.providers.Provider.getProviderForPurchase(Id(purchaseId)) match {
        case Some(provider) =>
//                  Ok(views.html.providers.invoicePDF(provider, purchase, packType, transaction))
          Ok(util.pdf.PDF.toBytes(views.html.providers.invoicePDF(provider, provider.currentPack.get.purchase, provider.currentPack.get.packType, provider.currentPack.get.purchase.transaction))).as("application/pdf")
        case None =>
          NotFound
      }
  }

  /**
   * Render the purchase info for the given invoice as a PDF file
   * @param purchaseId the purchase ID
   */
  def payInvoice(purchaseId: Long) = SecuredAction(IsOwnerOfServiceProviderPurchase(purchaseId)) {
    implicit request =>
      models.providers.Provider.getProviderForPurchase(Id(purchaseId)) match {
        case Some(provider) => {
          setExpressCheckout(provider)
        }
        case None =>
          NotFound
      }
  }

  /**
   * Handle PayPal response after the setExpressCheckout. Make a few check, save PayPal info and display the checkout
   * confirmation page
   * @param purchaseId the purchase ID
   */
  def checkoutInvoice(purchaseId: Long) = SecuredAction(IsOwnerOfServiceProviderPurchase(purchaseId)) {
    implicit request =>
      models.providers.Provider.getProviderForPurchase(Id(purchaseId)) match {
        case Some(provider) => {
          Cache.get(PURCHASE_CACHE_KEY) match {
            case Some(transaction: ProviderTransaction) => {
              val params = parseParameters(request.rawQueryString)
              val token = params.get("token")
              val payerId = params.get("PayerID")
              transaction.token match {
                case Some(t) => {
                  t.equals(token.getOrElse("")) match {
                    case true => {
                      val checkoutTransaction = transaction.copy(payerId = payerId)
                      Cache.set(PURCHASE_CACHE_KEY, checkoutTransaction, 15*60)
                      Ok(views.html.providers.checkout(provider, provider.currentPack.get.purchase, provider.currentPack.get.packType, checkoutTransaction))
                    }
                    case false => {
                      Logger.error("Providers ExpressCheckout data fill by user has failed (wrong token): "+params.toSeq.toString+" / "+transaction.toString);
                      gotoInvoice(purchaseId).flashing("error" -> Messages("main.payment.wrongToken"))
                    }
                  }
                }
                case None => {
                  Logger.error("Providers ExpressCheckout data fill by user has failed (no token in Transaction): "+params.toSeq.toString+" / "+transaction.toString);
                  gotoInvoice(purchaseId).flashing("error" -> Messages("main.payment.noTokenInPayment"))
                }
              }
            }
            case _ => {
              Logger.error("Providers ExpressCheckout data fill by user has failed: timeout.");
              gotoInvoice(purchaseId).flashing("error" -> Messages("main.payment.errorTimeOut"))
            }
          }
        }
        case None =>
          NotFound
      }
  }

  /**
   * Handle user confirmation to process the invoice payment (transaction completion)
   * @param purchaseId the purchase ID
   */
  def doPayInvoice(purchaseId: Long) = SecuredAction(IsOwnerOfServiceProviderPurchase(purchaseId)) {
    implicit request =>
      models.providers.Provider.getProviderForPurchase(Id(purchaseId)) match {
        case Some(provider) => {
          Cache.get(PURCHASE_CACHE_KEY) match {
            case Some(transaction: ProviderTransaction) => {
              doExpressCheckout(provider, transaction, user.get)
            }
            case _ => {
              Logger.error("Providers ExpressCheckout confirmation by user has failed: timeout.");
              gotoInvoice(purchaseId).flashing("error" -> Messages("main.payment.errorTimeOut"))
            }
          }
        }
        case None =>
          NotFound
      }
  }

  /**
   * Validate a service provider
   * @param id the service provider ID
   */
  def validate(id: Long) = SecuredAction(IsAdministrator()) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) => {
          models.providers.Provider.editProviderValidity(Id(id), true)
          val view = views.html.admin.notification.providers.mailNotificationProviderApproved(provider)
          val message = view.body
          NotificationCenter.notificationMail(provider.email, Messages("main.providers.subscriptionApproved.title"), message)
          gotoEdition(id).flashing("success" -> Messages("main.providers.successSubscriptionEdited"))
        }
        case None => NotFound
      }
  }

  /**
   * Invalidate a service provider
   * @param id the service provider ID
   */
  def invalidate(id: Long) = SecuredAction(IsAdministrator()) {
    implicit request =>
      models.providers.Provider.getProvider(Id(id)) match {
        case Some(provider) => {
          models.providers.Provider.editProviderValidity(Id(id), false)
          val view = views.html.admin.notification.providers.mailNotificationProviderRejected(provider)
          val message = view.body
          NotificationCenter.notificationMail(provider.email, Messages("main.providers.subscriptionRejected.title"), message)
          gotoEdition(id).flashing("success" -> Messages("main.providers.successSubscriptionEdited"))
        }
        case None => NotFound
      }
  }

  /**
   * Return the options for the select element of the categories
   */
  def getCategories(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Seq[(String, String)] = {
    models.providers.Provider.getProviderCategories.map(_.categoryId.get.toString).map(catId => catId -> Messages("main.providers.name."+catId))
  }

  /**
   * Return the category name given a category ID
   * @param categoryId the category ID
   */
  def getCategoryName(categoryId: Long)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = {
    models.providers.Provider.getProviderCategories.find(_.categoryId.get == categoryId) match {
      case Some(provider) => Some(provider.name)
      case None => None
    }
  }

  /**
   * Initialize the PayPal Express Checkout and when done redirect the user to PayPal
   * @param provider the purchase provider info
   */
  protected def setExpressCheckout(provider: models.providers.Provider)(implicit request: play.api.mvc.RequestHeader) = {
    val purchase = provider.currentPack.get.purchase
    val packType = provider.currentPack.get.packType

    val params = mutable.HashMap[String, String]()
    val method = "SetExpressCheckout"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val style = play.Play.application().configuration().getString("paypal.style")
    val returnUrl = controllers.providers.routes.Provider.checkoutInvoice(purchase.id.get).absoluteURL(isSecureConnection)
    val cancelUrl = controllers.providers.routes.Provider.invoice(purchase.id.get).absoluteURL(isSecureConnection)
    val amount = purchase.amount.toString
    val currency = purchase.currency
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")
    val providerInfo = Messages("main.providers.serviceProvider") + " " + provider.title
    val packName = Messages("main.providers.pack")+" "+packType.name
    val packDescription = Messages("main.providers.validFromToNbMonths", UtilsHelper.dateToString(purchase.since), UtilsHelper.dateToString(purchase.until), packType.nbMonths)

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
    params.put("PAYMENTREQUEST_0_AMT", amount)
    params.put("PAYMENTREQUEST_0_CURRENCYCODE", currency)
    params.put("PAYMENTREQUEST_0_CUSTOM", providerInfo)
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("GIFTMESSAGEENABLE", "0")
    params.put("GIFTRECEIPTENABLE", "0")
    params.put("GIFTWRAPENABLE", "0")
    params.put("L_PAYMENTREQUEST_0_NAME0", packName)
    params.put("L_PAYMENTREQUEST_0_DESC0", packDescription)
    params.put("L_PAYMENTREQUEST_0_AMT0", amount)
    params.put("L_PAYMENTREQUEST_0_QTY0", "1")

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
          val transaction = ProviderTransaction(purchase.id.get, purchase.amount, purchase.currency, Some(token), Some(correlationId), None, None, None)
          Cache.set(PURCHASE_CACHE_KEY, transaction, 15*60)
          Redirect(play.Play.application().configuration().getString("paypal.httpUrl") + "/webscr?cmd=_express-checkout&token=" + token)
        } else {
          Logger.error("SetExpressCheckoutSP has failed");
          getAndLogPayPalError(result)
          gotoInvoice(purchase.id.get).flashing("error" -> Messages("main.payment.paypalError"))
        }
      }
    }
  }

  /**
   * Finalize the PayPal Express Checkout and then redirect the user to a confirmation page
   * @param provider the purchase provider info
   * @param transaction the current transaction info
   */
  protected def doExpressCheckout(provider: models.providers.Provider, transaction: ProviderTransaction, user: User)(implicit request: play.api.mvc.RequestHeader) = {
    val purchase = provider.currentPack.get.purchase
    val packType = provider.currentPack.get.packType

    val params = mutable.HashMap[String, String]()
    val method = "DoExpressCheckoutPayment"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val style = play.Play.application().configuration().getString("paypal.style")
    val payerId = transaction.payerId
    val token = transaction.token
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")
    val amount = transaction.amount.toString
    val currency = transaction.currency
    val providerInfo = Messages("main.providers.serviceProvider") + " " + provider.title
    val packName = Messages("main.providers.pack")+" "+packType.name
    val packDescription = Messages("main.providers.validFromToNbMonths", UtilsHelper.dateToString(purchase.since), UtilsHelper.dateToString(purchase.until), packType.nbMonths)

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
    params.put("PAYMENTREQUEST_0_AMT", amount)
    params.put("PAYMENTREQUEST_0_CURRENCYCODE", currency)
    params.put("PAYMENTREQUEST_0_CUSTOM", providerInfo)
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("L_PAYMENTREQUEST_0_NAME0", packName)
    params.put("L_PAYMENTREQUEST_0_DESC0", packDescription)
    params.put("L_PAYMENTREQUEST_0_AMT0", amount)
    params.put("L_PAYMENTREQUEST_0_QTY0", "1")

    Async {
      WS.url(apiUrl).post(formatRequest(params)).map { response =>
        val result = parseParameters(response.body)

        val isSuccess = result.get("ACK") match {
          case Some(ack) => "Success".equals(ack)
          case None => false
        }

        if (isSuccess) {
          val completeTransaction = transaction.copy(transactionId = result.get("PAYMENTINFO_0_TRANSACTIONID"), accountId = result.get("PAYMENTINFO_0_SECUREMERCHANTACCOUNTID"))
          val paidPurchase = purchase.copy(
            paid = true,
            paymentDate = Some(new Date()),
            paymentMethod = Some(models.providers.Provider.PAYMENT_METHOD_PAYPAL)
          )
          models.providers.Provider.completePurchaseWithTransaction(paidPurchase, completeTransaction)
          val accountId = play.Play.application().configuration().getString("paypal.accountId")
          accountId.equals(completeTransaction.accountId.getOrElse("")) match {
            case true =>
              try {
                val view = views.html.admin.notification.providers.mailNotificationTransactionComplete(completeTransaction, provider, user)
                val message = view.body
                NotificationCenter.adminNotificationMail("[INFO] Paiement reçu (fournisseur)", message)
              } catch {
                case _ => Logger.error("Providers Could not send module payment mail to admin with data: "+completeTransaction.toString)
              }
              Cache.set(PURCHASE_CACHE_KEY, null, 0)
              gotoInvoice(transaction.purchaseId).flashing("success" -> Messages("main.providers.checkout.success"))
            case false =>
              Logger.error("Providers PayPal fraud detected in")
              Logger.error("\t"+ completeTransaction.toString)
              val mailSubject = "[IMPORTANT] PayPal fraud detection Providers"
              val warningMessage = "Il est possible que le site soit victime d'une tentative de détournement de fonds lors des paiements PayPal pour ajouter des crédits à son compte (fournisseur)"
              val view = views.html.admin.notification.providers.mailNotificationTransactionComplete(completeTransaction, provider, user, Some(warningMessage))
              val message = view.body
              NotificationCenter.emergencyMail(mailSubject, message)
              Cache.set(PURCHASE_CACHE_KEY, null, 0)
              gotoInvoice(transaction.purchaseId).flashing("error" -> Messages("main.payment.paypalFraudDetection"))
          }
        } else {
          Logger.error("Providers DoExpressCheckout has failed");
          getAndLogPayPalError(result)
          gotoInvoice(transaction.purchaseId).flashing("error" -> Messages("main.payment.paypalError"))
        }
      }
    }

  }

  /**
   * Resize a given logo picture file to match the logo display expectations (and reduce size before save)
   * @param logoPicture the logo info
   * @param originalFile the logo file
   * @return a resized logo file
   */
  protected def getResizedLogoPicture(logoPicture: models.providers.ProviderLogoPicture, originalFile: File): File = {
    val resizedLogoPicture = File.createTempFile("resized", null)
    ImagesHelper.resize(logoPicture.filename.getOrElse("logo.jpg"), originalFile, resizedLogoPicture, models.providers.Provider.LOGO_PICTURE_WIDTH, models.providers.Provider.LOGO_PICTURE_HEIGHT)
      resizedLogoPicture
  }

  /**
   * Resize a given showroom picture file to match the normal display expectations (and reduce size before save)
   * @param showroomPicture the picture info
   * @param originalFile the picture file
   * @return a resized picture file
   */
  protected def getResizedShowroomPicture(showroomPicture: models.providers.ProviderShowroomPicture, originalFile: File): File = {
    val resizedShowroomPicture = File.createTempFile("resized", null)
    ImagesHelper.resize(showroomPicture.filename.getOrElse("logo.jpg"), originalFile, resizedShowroomPicture, models.providers.Provider.SHOWROOM_PICTURE_WIDTH, models.providers.Provider.SHOWROOM_PICTURE_HEIGHT)
    resizedShowroomPicture
  }

  /**
   * Resize a given showroom picture file to match the thumbnail display expectations (and reduce size before save)
   * @param showroomPicture the picture info
   * @param originalFile the picture file
   * @return a resized picture file
   */
  protected def getThumbnailShowroomPicture(showroomPicture: models.providers.ProviderShowroomPicture, originalFile: File): File = {
    val thumbnailShowroomPicture = File.createTempFile("thumb", null)
    ImagesHelper.resize(showroomPicture.filename.getOrElse("thumb.jpg"), originalFile, thumbnailShowroomPicture, models.providers.Provider.SHOWROOM_THUMBNAIL_WIDTH, models.providers.Provider.SHOWROOM_THUMBNAIL_HEIGHT)
      thumbnailShowroomPicture
  }
}
