package controllers.admin

import com.typesafe.plugin._
import play.api.mvc.{Action, Controller}
import play.api.Play.current
import play.api.{Logger, Play}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import controllers.AnyController

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 15.02.13
 * Time: 15:30
 * To change this template use File | Settings | File Templates.
 */
object NotificationCenter extends AnyController {
  /**
   * Contact form definition
   */
  val contactForm = Form(
    tuple(
      "name" -> nonEmptyText,
      "email" -> email,
      "title" -> nonEmptyText,
      "message" -> nonEmptyText,
      "captchaUuid" -> nonEmptyText,
      "captchaValue" -> nonEmptyText
    ).verifying(Messages("main.captcha.captchaDoesNotMatch"), form => form match {
      case (name, email, title, message, captchaUuid, captchaValue) => validateCaptcha(captchaValue, captchaUuid)
    })
  )

  /**
   * Report bug form definition
   */
  val reportBugForm = Form(
    tuple(
      "name" -> optional(text),
      "email" -> optional(email),
      "title" -> nonEmptyText,
      "description" -> nonEmptyText,
      "referrer" -> optional(text),
      "captchaUuid" -> nonEmptyText,
      "captchaValue" -> nonEmptyText
    ).verifying(Messages("main.captcha.captchaDoesNotMatch"), form => form match {
      case (name, email, title, description, referrer, captchaUuid, captchaValue) => validateCaptcha(captchaValue, captchaUuid)
    })
  )

  /**
   * Display the contact form
   */
  def contact = Action {
    implicit request =>
      Ok(views.html.admin.notification.contact(contactForm))
  }

  /**
   * Handle contact form submission
   */
  def doContactMail = Action {
    implicit request => contactForm.bindFromRequest.fold(
      formWithErrors =>
        BadRequest(views.html.admin.notification.contact(formWithErrors)),
      contactFormOk => {
        val toAddress = play.Play.application().configuration().getString("administratorMail")
        val fromAddressAntiSpam = contactFormOk._1+" via click-wedding <"+play.Play.application().configuration().getString("notificationMail")+">"
        val fromAddress = contactFormOk._1+" <"+contactFormOk._2+">"
        val subject = "[CONTACT] "+contactFormOk._3
        val message = views.html.admin.notification.mailContact(contactFormOk._1, contactFormOk._2, contactFormOk._3, contactFormOk._4, new java.util.Date()).body
        try {
          sendMail(toAddress, fromAddressAntiSpam, subject, message, false, Some(fromAddress))
          Ok(views.html.admin.notification.success())
        } catch {
          case error => {
            Logger.error("[ERR] Contact message error: "+contactFormOk.toString)
            Logger.error("[ERR] "+error.toString)
            Redirect(routes.NotificationCenter.contact).flashing("error" -> Messages("main.notification.error"))
          }
        }
      }
    )
  }

  /**
   * Display the report bug form
   */
  def reportBug = Action {
    implicit request =>
    //      Ok(views.html.admin.notification.reportBug(reportBugForm.fill((None, None, "", "", request.headers.get(REFERER), "", ""))))
      Redirect("https://www.afidis.ch/click-wedding-support/")
  }

  /**
   * Handle bug report form submission
   */
  def doReportBug = Action {
    implicit request => reportBugForm.bindFromRequest.fold(
      formWithErrors =>
        BadRequest(views.html.admin.notification.reportBug(formWithErrors)),
      reportBugFormOk => {
        val toAddress = play.Play.application().configuration().getString("bugReportMail")
        val fromAddressAntiSpam = reportBugFormOk._1 match {
          case Some(name) => name +" via click-wedding <"+play.Play.application().configuration().getString("notificationMail")+">"
          case None => "Anonymous user via click-wedding <"+play.Play.application().configuration().getString("notificationMail")+">"
        }
        val fromAddress = reportBugFormOk._2 match {
          case Some(email) => reportBugFormOk._1 match {
            case Some(name) =>
              name+" <"+email+">"
            case _ =>
              email
          }
          case _ => toAddress
        }
        val subject = "[BUG] "+reportBugFormOk._3
        val message = views.html.admin.notification.mailReportBug(reportBugFormOk._1, reportBugFormOk._2, reportBugFormOk._3, reportBugFormOk._4, reportBugFormOk._5, new java.util.Date()).body
        try {
          sendMail(toAddress, fromAddressAntiSpam, subject, message, false, Some(fromAddress))
          Ok(views.html.admin.notification.success())
        } catch {
          case error => {
            Logger.error("[ERR] Report bug error: "+reportBugFormOk.toString)
            Logger.error("[ERR] "+error.toString)
            Redirect(routes.NotificationCenter.reportBug).flashing("error" -> Messages("main.notification.error"))
          }
        }
      }
    )
  }

  /**
   * Generate a sitemap XML file
   */
  def generateSitemap = Action {
    implicit request =>
      Ok(views.xml.sitemap(List(
        controllers.routes.Wedding.index,
        controllers.routes.Wedding.welcome,
        controllers.routes.Wedding.create,
        controllers.routes.Wedding.search,
        controllers.admin.routes.NotificationCenter.contact,
        controllers.admin.routes.HelpCenter.terms,
        controllers.admin.routes.HelpCenter.privacy,
        controllers.admin.routes.HelpCenter.faq,
        securesocial.controllers.routes.LoginPage.login,
        securesocial.controllers.routes.Registration.startSignUp
      )))
  }

  /**
   * Method called on emergency to send a high-priority mail (e.g. fraud detection with PayPal)
   * @param title the message title
   * @param message the message body
   */
  def emergencyMail(title: String, message: String) {
    val toAddress = play.Play.application().configuration().getString("emergencyMail")
    val fromAddress = play.Play.application().configuration().getString("notificationMail")
    sendMail(toAddress, fromAddress, title, message, true)
  }

  /**
   * Method called to notify the administrator
   * @param title the message title
   * @param message the message body
   */
  def adminNotificationMail(title: String, message: String) {
    val toAddress = play.Play.application().configuration().getString("notificationMail")
    notificationMail(toAddress, title, message)
  }

  /**
   * Methode called to notify someone of something
   * @param recipientAddress the recipient address
   * @param title the message title
   * @param message the message body
   */
  def notificationMail(recipientAddress: String, title: String, message: String) {
    val fromAddress = play.Play.application().configuration().getString("notificationMail")
    sendMail(recipientAddress, fromAddress, title, message, false)
  }

  /**
   * Send a mail
   * @param recipientMail the recipient mail address
   * @param senderMail the sender mail address
   * @param subject the subject
   * @param message the body of the mail
   * @param sendCopy set to true to send a copy to the sender
   * @param replyTo optional reply to mail address
   */
  def sendMail(recipientMail: String, senderMail: String, subject: String, message: String, sendCopy: Boolean, replyTo: Option[String] = None) {
    val mail = use[MailerPlugin].email
    mail.setSubject(subject)
    mail.addRecipient(recipientMail)
    if (replyTo.isDefined) {
      mail.setReplyTo(replyTo.get)
    }
    if (sendCopy) {
      if (replyTo.isDefined) {
        mail.addBcc(replyTo.get)
      } else {
        mail.addBcc(senderMail)
      }
    }
    mail.addFrom(senderMail)
    mail.send(message, message)
  }
}
