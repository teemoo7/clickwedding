package controllers.modules

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 27.11.12
 * Time: 16:53
 * To change this template use File | Settings | File Templates.
 */

import play.api.mvc.Action
import controllers._
import admin.NotificationCenter
import play.api.data.Form
import play.api.data.Forms._
import anorm.{Pk, NotAssigned}
import play.api.i18n.Messages
import anorm.Id
import scala.Some

object Contact extends AnyController {

  /**
   * Mail form definition
   */
  val mailForm = Form(
    tuple (
      "recipient" -> number,
      "firstName" -> nonEmptyText(maxLength = 50),
      "lastName" -> nonEmptyText(maxLength = 50),
      "mailAddress" -> nonEmptyEmail,
      "subject" -> nonEmptyText(maxLength = 150),
      "message" -> nonEmptyText(maxLength = 500),
      "copy" -> boolean,
      "captchaUuid" -> nonEmptyText(maxLength = 50),
      "captchaValue" -> nonEmptyText(maxLength = 50)
    ).verifying(Messages("main.captcha.captchaDoesNotMatch"), form => form match {
      case (recipient, firstName, lastName, mailAddress, subject, message, copy, captchaUuid, captchaValue) => validateCaptcha(captchaValue, captchaUuid)
    })
  )

  /**
   * Contact infos form definition
   */
  val contactForm = Form(
    mapping (
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "contactInfo" -> mapping (
        "coupleMail" -> optional(email).verifying(Messages("error.maxLength", 150), o => o.getOrElse("").length <= 150),
        "couplePhone" -> optional(text(maxLength = 30)),
        "coupleAddressName" -> optional(text(maxLength = 100)),
        "coupleAddressStreet" -> optional(text(maxLength = 100)),
        "coupleAddressZip" -> optional(text(maxLength = 10)),
        "coupleAddressPlace" -> optional(text(maxLength = 100)),
        "organizerMail" -> optional(email).verifying(Messages("error.maxLength", 150), o => o.getOrElse("").length <= 150),
        "organizerPhone" -> optional(text(maxLength = 30)),
        "organizerName" -> optional(text(maxLength = 100))
      ) (models.modules.ContactInfo.apply)(models.modules.ContactInfo.unapply)
    )
      (
        (weddingId, contactInfo) =>
          models.modules.Contact(weddingId, Id(models.modules.Contact.ID), models.modules.Contact.NAME, contactInfo, true, None, None, None, None)
      )
      (
        (contact: models.modules.Contact) =>
          Some(contact.weddingId, models.modules.ContactInfo(contact.moduleContent.coupleMail, contact.moduleContent.couplePhone, contact.moduleContent.coupleAddressName,
            contact.moduleContent.coupleAddressStreet, contact.moduleContent.coupleAddressZip, contact.moduleContent.coupleAddressPlace, contact.moduleContent.organizerMail, contact.moduleContent.organizerPhone))
      )
  )


  /**
   * Contact instructions options form definition
   */
  val contactInstructionsForm = Form(
    "instructions" -> optional(text(minLength = 0, maxLength = 300))
  )

  /**
   * Display an HTML template with the contact infos
   */
  def getContact(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader) = {
    loadContact(wedding) match {
      case Some(contact) =>
        views.html.modules.contact.display(wedding, contact)
      case _ =>
    }
  }

  /**
   * Display a summarized HTML template with the basic contact infos
   */
  def getContactSummary(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader) = {
    loadContact(wedding) match {
      case Some(contact) =>
        views.html.modules.contact.summary(wedding, contact)
      case _ =>
    }
  }

  /**
   * Display the contact standalone page
   */
  def display(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          loadContact(wedding).map {
            contact =>
              Ok(views.html.modules.contact.displayFull(wedding, contact))
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Load the contact module given a wedding
   * @param wedding
   * @return the contact module if found
   */
  protected def loadContact(wedding: models.wedding.Wedding): Option[models.modules.Contact] = {
    wedding.modules.find(module => module.getId.get == models.modules.Contact.ID) match {
      case Some(module) =>
        module match {
          case contact: models.modules.Contact =>
            Some(contact)
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  /**
   * Display the mail form for the given wedding
   * @param uid the wedding UID
   */
  def mail(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          loadContact(wedding).map {
            contact =>
              Ok(views.html.modules.contact.mail(wedding, contact, mailForm))
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Mail submission handler, send the mail
   * @param uid the wedding UID
   */
  def doMail(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          loadContact(wedding).map {
            contact =>
              mailForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest(views.html.modules.contact.mail(wedding, contact, formWithErrors)).flashing("error" -> Messages("main.modules.contact.missingInfo")),
                mailForm => {
                  val recipient = mailForm._1 match {
                    case models.modules.Contact.RECIPIENT_COUPLE => contact.moduleContent.coupleMail.get
                    case models.modules.Contact.RECIPIENT_ORGANIZER => contact.moduleContent.organizerMail.get
                    case _ => ""
                  }
                  val firstName = mailForm._2
                  val lastName = mailForm._3
                  val sender = firstName.concat(" ").concat(lastName).concat(" <").concat(mailForm._4).concat(">")
                  val subject = mailForm._5
                  val message = mailForm._6
                  val sendCopy = mailForm._7

                  val fromAddressAntiSpam = firstName+" "+lastName+" via click-wedding <"+play.Play.application().configuration().getString("notificationMail")+">"

                  NotificationCenter.sendMail(recipient, fromAddressAntiSpam, subject, message, sendCopy, Some(sender))
                  Wedding.goToCurrentWedding(uid).flashing("success" -> Messages("main.modules.contact.mailSuccessfullySent"))
                }
            )
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Display edit contact info form
   * @param uid the wedding UID
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(m => m.getId.get.toInt == models.modules.Contact.ID) match {
//            models.modules.Contact.load(wedding.id) match {
              case Some(contact: models.modules.Contact) =>
                Ok(views.html.modules.contact.edit(wedding, contactForm.fill(contact), contactInstructionsForm.fill(contact.instructions)))
              case _ =>
                NotFound
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
            wedding.modules.find(m => m.getId.get.toInt == models.modules.Contact.ID) match {
//            models.modules.Contact.load(wedding.id) match {
              case Some(contact: models.modules.Contact) =>
                contactForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest(views.html.modules.contact.edit(wedding, formWithErrors, contactInstructionsForm.fill(contact.instructions))),
                  contactForm => {
                    val contactWithWeddingId = contactForm.copy(weddingId = wedding.id)
                    models.modules.Contact.edit(contactWithWeddingId)
                    Wedding.goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.modules.contact.modificationsSaved"))
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
          wedding.modules.find(m => m.getId.get.toInt == models.modules.Contact.ID) match {
//          models.modules.Contact.load(wedding.id) match {
            case Some(contact: models.modules.Contact) =>
              contactInstructionsForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest(views.html.modules.contact.edit(wedding, contactForm.fill(contact), formWithErrors)),
                instructions => {
                  models.modules.Contact.editContactInstructions(contact.copy(instructions = instructions))
                  controllers.Wedding.goToCurrentWedding(uid).flashing("success" -> Messages("main.modules.contact.modificationsSaved"))
                }
              )
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }
}
