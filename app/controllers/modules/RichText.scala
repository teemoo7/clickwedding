package controllers.modules

import controllers.{AnyController, CookieLang, Wedding}
import play.api.data.Form
import play.api.data.Forms._
import anorm.{Id, Pk, NotAssigned}
import org.owasp.validator.html._
import play.api.Play.current
import play.api.Play
import play.api.i18n.Messages

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 16.11.12
 * Time: 15:05
 * To change this template use File | Settings | File Templates.
 */
object RichText extends AnyController {
  val POLICY_FILE_LOCATION = "antisamy-myspace-1.4.4.xml"
//  val POLICY_FILE_LOCATION = "antisamy-tinymce-1.4.4.xml"

  /**
   * RichText form definition
   */
  val richTextForm = Form(
    mapping (
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "content" -> text(maxLength = 65535)
    )
      (
        (weddingId, content) => models.modules.RichText(weddingId, Id(models.modules.RichText.ID), models.modules.RichText.NAME, content, true, None, None, None)
      )
      (
        (richText: models.modules.RichText) => Some(richText.weddingId, richText.moduleContent)
      )
  )

  /**
   * Display edit rich text form
   * @param uid
   * @return
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.RichText.load(wedding.id).map {
              richText =>
                Ok(views.html.modules.richText.edit(wedding, richTextForm.fill(richText)))
            }.getOrElse(NotFound)
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle edit form submission
   * @param uid
   * @return
   */
  def doEdit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            richTextForm.bindFromRequest.fold(
              formWithErrors =>
                BadRequest(views.html.modules.richText.edit(wedding, formWithErrors)),
              richText => {
                    val richTextWedding = models.modules.RichText(wedding.id, Id(models.modules.RichText.ID), models.modules.RichText.NAME, checkAntiSammy(richText.moduleContent), true, None, None, None)
                    models.modules.RichText.edit(richTextWedding)
                    Wedding.goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.modules.richText.modificationsSaved"))
                }
            )
          }
        }.getOrElse(NotFound)
  }

  /**
   * Clean an HTML input text with the lib AntiSamy
   * @param inputHtml the input html source to be cleaned
   * @return a purified html text
   */
  def checkAntiSammy(inputHtml: String): String = {
    val policy = Policy.getInstance(Play.getFile("conf/"+POLICY_FILE_LOCATION))
//    val policy = Policy.getInstance(Play.application.classloader.getResource(POLICY_FILE_LOCATION).getFile)
    val as = new AntiSamy;
    val cr = as.scan(inputHtml, policy);
    cr.getCleanHTML
  }
}