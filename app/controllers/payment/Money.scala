package controllers.payment

import play.api.i18n.{Lang, Messages}
import collection.mutable
import play.api.libs.ws.WS
import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import models.payment.{Promotion, Payment}
import anorm.{Id, NotAssigned}
import controllers.AnyController
import play.api.cache.Cache
import play.api.Play.current
import controllers.helpers.WSHelper._
import models.authentication.User
import play.api.libs.concurrent.Execution.Implicits._
import play.api.data.format.Formats._
import controllers.admin.NotificationCenter
import views.html.helper
import play.api.mvc.Action
import java.util.Date

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.01.13
 * Time: 17:31
 * To change this template use File | Settings | File Templates.
 */
object Money extends AnyController {

  val CHF = "CHF"
  val EUR = "EUR"
  val USD = "USD"
  val currencies = Array(CHF)
//  val currencies = Array(CHF, EUR, USD)
  val exchangeRates = mutable.HashMap(CHF -> 1.00, EUR -> 1.20, USD -> 0.80)

  val ACTION_OPENING = 0
  val ACTION_PAYMENT = 1
  val ACTION_PURCHASE = 2
  val ACTION_PROMOTION = 3

  val OPENING_BONUS_AMOUNT = 15

  val PAYMENT_CACHE_KEY = "PayPalExpressCheckoutPayment"


  /**
   * Add money to wedding form
   */
  val buyMoneyForm = Form(
    mapping (
      "moneyAmount" -> number(1, 10000),
      "totalCost" -> of[Double],
      "currency" -> nonEmptyText.verifying(Messages("main.payment.wrongCurrency"), currency => currencies.contains(currency)),
      "conditions" -> checked(Messages("main.payment.pleaseAcceptTermsAndConditions"))
    )
      ((moneyAmount, totalCost, currency, _) => Payment(NotAssigned, moneyAmount, totalCost, currency, None, None, None, None, None, null, -1))
      ((payment: Payment) => Some(payment.moneyAmount, payment.totalCost, payment.currency, false))
      verifying(Messages("main.payment.wrongTotalCost"), payment => checkPaymentTotal(payment))
  )

  /**
   * Promotion code form
   */
  val promotionForm = Form(
    mapping (
      "code" -> nonEmptyText(maxLength = 10)
    )
      ((code) => Promotion(NotAssigned, code, 0, None, None, None))
      ((promotion: Promotion) => Some(promotion.code))
      verifying(Messages("main.payment.wrongCode"), promotion => checkPromotion(promotion))
  )

  /**
   * Promotion edition form
   */
  val editPromotionForm = Form(
    mapping (
      "id" -> number,
      "code" -> nonEmptyText(maxLength = 10),
      "amount" -> number,
      "remaining" -> optional(number),
      "deadline" -> optional(date),
      "weddingId" -> optional(number)
    )
      ((id, code, amount, remaining, deadline, weddingId) => models.payment.Promotion(Id(id), code, amount, remaining, deadline, weddingId match {case Some(i) => Some(i.toLong) case None => None}))
      ((promotion: models.payment.Promotion) => Some(promotion.id.get.toInt, promotion.code, promotion.amount, promotion.remaining, promotion.deadline, promotion.weddingId match {case Some(l) => Some(l.toInt) case None => None}))
  )

  /**
   * Display a form to manage money for the wedding
   * @param uid the wedding UID
   */
  def display(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => Ok(views.html.payment.money(wedding, buyMoneyForm.fill(Payment(NotAssigned, 0, 0.00f, CHF, None, None, None, None, None, wedding.uid, -1)), promotionForm))
      }.getOrElse(NotFound)
  }

  /**
   * Handle buy money form submission and deals with the Paypal service
   * @param uid the wedding UID
   */
  def buy(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          buyMoneyForm.bindFromRequest.fold(
            formWithErrors => BadRequest(views.html.payment.money(wedding, formWithErrors, promotionForm)).flashing("error" -> Messages("main.payment.wrongInfo")),
            payment => {
              setExpressCheckout(uid, payment.copy(weddingId = wedding.id.get, weddingUid = uid))
            }
        )
      }.getOrElse(NotFound)
  }

  /**
   * Callback URL when after the user filled the PayPal checkout. Display the confirmation page to pay now.
   * @param uid the wedding UID
   */
  def checkout(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          Cache.get(PAYMENT_CACHE_KEY) match {
            case Some(payment: Payment) => {
              val params = parseParameters(request.rawQueryString)
              val token = params.get("token")
              val payerId = params.get("PayerID")
              payment.token match {
                case Some(t) => {
                  t.equals(token.getOrElse("")) match {
                    case true => {
                      val checkoutPayment = payment.copy(payerId = payerId)
                      Cache.set(PAYMENT_CACHE_KEY, checkoutPayment, 15*60)
                      Ok(views.html.payment.checkout(wedding, checkoutPayment))
                    }
                    case false => {
                      Logger.error("ExpressCheckout data fill by user has failed (wrong token): "+params.toSeq.toString+" / "+payment.toString);
                      goToDisplayMoney(uid).flashing("error" -> Messages("main.payment.wrongToken"))
                    }
                  }
                }
                case None => {
                  Logger.error("ExpressCheckout data fill by user has failed (no token in Payment): "+params.toSeq.toString+" / "+payment.toString);
                  goToDisplayMoney(uid).flashing("error" -> Messages("main.payment.noTokenInPayment"))
                }
              }
            }
            case _ => {
              Logger.error("ExpressCheckout data fill by user has failed: timeout.");
              goToDisplayMoney(uid).flashing("error" -> Messages("main.payment.errorTimeOut"))
            }
          }
      }.getOrElse(NotFound)
  }

  /**
   * Handle checkout confirmation, then call doExpressCheckout to finalize the payment
   * @param uid the wedding UID
   */
  def doPay(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          Cache.get(PAYMENT_CACHE_KEY) match {
            case Some(payment: Payment) => {
              doExpressCheckout(wedding, payment, user.get)
            }
            case _ => {
              Logger.error("ExpressCheckout confirmation by user has failed: timeout.");
              goToDisplayMoney(uid).flashing("error" -> Messages("main.payment.errorTimeOut"))
            }
          }
      }.getOrElse(NotFound)
  }

  /**
   * Handle promotion code form and apply it if available
   * @param uid
   * @return
   */
  def check(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          promotionForm.bindFromRequest.fold(
            formWithErrors =>
              //goToDisplayMoney(uid).flashing("error" -> Messages("main.payment.wrongCode")),
              BadRequest(views.html.payment.money(wedding, buyMoneyForm.fill(Payment(NotAssigned, 0, 0.00f, CHF, None, None, None, None, None, wedding.uid, -1)), formWithErrors)),
            promotion => {
              getPromotion(promotion) match {
                case Some(promotionFound) => {
                  models.payment.Money.addPromotion(wedding, user.get, promotionFound)
                  goToListTransaction(uid).flashing("success" -> promotionFound.amount.toString.concat(" ").concat(Messages("main.payment.success")))
                }
                case None => NotFound
              }
            }
          )
      }.getOrElse(NotFound)
  }

  /**
   * Display the list of the last transactions for the wedding
   * @param uid the wedding UID
   */
  def listTransactions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => Ok(views.html.payment.transactions(wedding, models.payment.Money.getAllTransactions(wedding.id)))
      }.getOrElse(NotFound)
  }

  /**
   * Display the invoice of a payment with printer-ready styling
   * @param uid the wedding UID
   * @param id the transaction ID
   */
  def invoice(uid: String, id: Long) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => models.payment.Money.getTransaction(id).map {
          transaction => transaction.weddingId == wedding.id.get match {
            case true =>
              transaction.action match {
                case payment: models.payment.Payment => {
                  Ok(views.html.payment.invoice(transaction, payment))
                }
                case _ => BadRequest
              }
            case false =>
              BadRequest
          }
        }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Send the invoice of a payment as a PDF file
   * @param uid the wedding UID
   * @param id the transaction ID
   */
  def invoicePDF(uid: String, id: Long) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => models.payment.Money.getTransaction(id).map {
          transaction => transaction.weddingId == wedding.id.get match {
            case true =>
              transaction.action match {
                case payment: models.payment.Payment => {
                  Ok(util.pdf.PDF.toBytes(views.html.payment.invoicePDF.render(transaction, payment, flash, request, user, lang))).as("application/pdf")
                }
                case _ => BadRequest
              }
            case false =>
              BadRequest
          }
        }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * List all the promotion codes
   */
  def listPromotion = SecuredAction(IsAdministrator()) {
    implicit request =>
      Ok(views.html.payment.displayPromotionAdmin(models.payment.Money.getAllPromotion))
  }

  /**
   * Edit a promotion
   */
  def editPromotion(id: Long) = SecuredAction(IsAdministrator()) {
    implicit request =>
      models.payment.Money.getPromotion(id).map {
        promotion =>
          Ok(views.html.payment.editPromotionAdmin(editPromotionForm.fill(promotion)))
      }.getOrElse(NotFound)
  }

  /**
   * Save the promotion edition
   */
  def doEditPromotion = SecuredAction(IsAdministrator()) {
    implicit request =>
      editPromotionForm.bindFromRequest.fold(
        formWithErrors =>
          BadRequest(views.html.payment.editPromotionAdmin(formWithErrors)),
        promotion => {
          models.payment.Money.getPromotion(promotion.id.get) match {
            case Some(promotionFound) => {
              models.payment.Money.updatePromotion(promotion)
              Ok(views.html.payment.displayPromotionAdmin(models.payment.Money.getAllPromotion)).flashing("success" -> Messages("main.payment.success"))
            }
            case None => NotFound
          }
        }
      )
  }

  /**
   * Initialize the PayPal Express Checkout and when done redirect the user to PayPal
   * @param payment the payment info
   * @param uid the wedding UID
   */
  protected def setExpressCheckout(uid: String, payment: Payment)(implicit request: play.api.mvc.RequestHeader) = {
    val params = mutable.HashMap[String, String]()
    val method = "SetExpressCheckout"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val style = play.Play.application().configuration().getString("paypal.style")
    val returnUrl = controllers.payment.routes.Money.checkout(uid).absoluteURL(isSecureConnection)
    val cancelUrl = controllers.payment.routes.Money.display(uid).absoluteURL(isSecureConnection)
    val amount = payment.totalCost.toString
    val money = payment.moneyAmount.toString
    val currency = payment.currency
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")

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
    params.put("PAYMENTREQUEST_0_CUSTOM", Messages("main.wedding.wedding") + " " + uid)
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("GIFTMESSAGEENABLE", "0")
    params.put("GIFTRECEIPTENABLE", "0")
    params.put("GIFTWRAPENABLE", "0")
    params.put("L_PAYMENTREQUEST_0_NAME0", money + " " + Messages("main.payment.money"))
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
          val checkoutPayment = payment.copy(token = Some(token), correlationId = Some(correlationId))
          Cache.set(PAYMENT_CACHE_KEY, checkoutPayment, 15*60)
          Redirect(play.Play.application().configuration().getString("paypal.httpUrl") + "/webscr?cmd=_express-checkout&token=" + token)
        } else {
          Logger.error("SetExpressCheckout has failed");
          getAndLogPayPalError(result)
          goToDisplayMoney(uid).flashing("error" -> Messages("main.payment.paypalError"))
        }
      }
    }
  }

  /**
   * Finalize the PayPal Express Checkout and then redirect the user to a confirmation page
   * @param wedding the wedding
   * @param payment the payment info
   */
  protected def doExpressCheckout(wedding: models.wedding.Wedding, payment: Payment, user: User)(implicit request: play.api.mvc.RequestHeader) = {
    val params = mutable.HashMap[String, String]()
    val method = "DoExpressCheckoutPayment"
    val version = play.Play.application().configuration().getString("paypal.version")
    val username = play.Play.application().configuration().getString("paypal.username")
    val password = play.Play.application().configuration().getString("paypal.password")
    val email = play.Play.application().configuration().getString("paypal.email")
    val signature = play.Play.application().configuration().getString("paypal.signature")
    val style = play.Play.application().configuration().getString("paypal.style")
    val amount = payment.totalCost.toString
    val money = payment.moneyAmount.toString
    val currency = payment.currency
    val payerId = payment.payerId
    val token = payment.token
    val apiUrl = play.Play.application().configuration().getString("paypal.apiUrl")

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
    params.put("PAYMENTREQUEST_0_CUSTOM", Messages("main.wedding.wedding") + " " + wedding.uid)
    params.put("PAYMENTREQUEST_0_PAYMENTACTION", "Sale")
    params.put("L_PAYMENTREQUEST_0_NAME0", money + " " + Messages("main.payment.money"))
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
          val completePayment = payment.copy(transactionId = result.get("PAYMENTINFO_0_TRANSACTIONID"), accountId = result.get("PAYMENTINFO_0_SECUREMERCHANTACCOUNTID"))
          val tId: Long = models.payment.Money.addPayment(wedding, user, completePayment)
          val accountId = play.Play.application().configuration().getString("paypal.accountId")
          accountId.equals(completePayment.accountId.getOrElse("")) match {
            case true =>
              try {
                val view = views.html.payment.mailNotification(completePayment, user, tId)
                val message = view.body
                NotificationCenter.adminNotificationMail("[INFO] Paiement reçu", message)
              } catch {
                case _ => Logger.error("Could not send module payment mail to admin with data: "+completePayment.toString)
              }
              Cache.set(PAYMENT_CACHE_KEY, null, 0)
              goToListTransaction(wedding.uid).flashing("success" -> money.concat(" ").concat(Messages("main.payment.success")).concat(" ").concat(Messages("main.payment.successInvoice")))
            case false =>
              Logger.error("PayPal fraud detected in")
              Logger.error("\t"+ completePayment.toString)
              val mailSubject = "[IMPORTANT] PayPal fraud detection"
              val warningMessage = "Il est possible que le site soit victime d'une tentative de détournement de fonds lors des paiements PayPal pour ajouter des crédits à son compte."
              val view = views.html.payment.mailNotification(completePayment, user, tId, Some(warningMessage))
              val message = view.body
              NotificationCenter.emergencyMail(mailSubject, message)
              Cache.set(PAYMENT_CACHE_KEY, null, 0)
              goToDisplayMoney(wedding.uid).flashing("error" -> Messages("main.payment.paypalFraudDetection"))
          }
        } else {
          Logger.error("DoExpressCheckout has failed");
          getAndLogPayPalError(result)
          goToDisplayMoney(wedding.uid).flashing("error" -> Messages("main.payment.paypalError"))
        }
      }
    }

  }

  /**
   * Check if the total cost of a payment corresponds to what was expected given the currency exchange rate
   * @param payment the payment to check
   * @return true if the total cost is OK
   */
  protected def checkPaymentTotal(payment: Payment): Boolean = {
    if (exchangeRates.contains(payment.currency)) {
      val rate = exchangeRates.get(payment.currency).get
      payment.moneyAmount.toFloat / rate == payment.totalCost
    } else {
      false
    }
  }

  /**
   * Check if a promotion corresponds to the given code and if it was not already used
   * @param promotion
   * @return true if the promotion exists and can be used
   */
  protected def checkPromotion(promotion: Promotion): Boolean = {
    getPromotion(promotion) match {
      case Some(promo) => {
        promo.isValid
      }
      case None => false
    }
  }

  /**
   * Get the promotion given its code
   * @param promotion
   * @return the promotion if exists
   */
  protected def getPromotion(promotion: Promotion): Option[Promotion] = {
    models.payment.Money.getPromotionByCode(promotion.code)
  }

  /**
   * Redirect to the display of the money of the wedding
   */
  protected def goToDisplayMoney(uid: String) = Redirect(controllers.payment.routes.Money.display(uid))

  /**
   * Redirect to the list of the transactions
   */
  protected def goToListTransaction(uid: String) = Redirect(controllers.payment.routes.Money.listTransactions(uid))
}
