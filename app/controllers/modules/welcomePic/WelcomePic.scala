package controllers.modules.welcomePic

import controllers.AnyController
import play.api.mvc.Action
import play.api.data.Form
import play.api.data.Forms._
import anorm.{Id, Pk, NotAssigned}
import play.api.i18n.Messages
import java.util.Date
import play.api.Routes
import app.controllers.helpers.ImagesHelper
import java.io.File
import controllers.helpers.UtilsHelper

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 17.04.13
 * Time: 11:39
 * To change this template use File | Settings | File Templates.
 */
object WelcomePic extends AnyController {

  /**
   * Picture options form definition
   */
  val optionsForm = Form(
    tuple (
      "showName" -> boolean,
      "name" -> optional(text(maxLength = 40))
    )
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.WelcomePic.addWelcomePictureAjax,
        routes.javascript.WelcomePic.deleteWelcomePictureAjax
      )
    ).as("text/javascript")
  }

  /**
   * Display the admin console of the welcome pic
   * @param uid the wedding UID
   */
  def editWelcomePic(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          models.modules.WelcomePic.loadWelcomePic(wedding.id).map {
            welcomePic =>
              Ok(views.html.modules.welcomePic.edit(wedding, welcomePic.moduleContent, optionsForm.fill((welcomePic.moduleContent.showName, welcomePic.moduleContent.name))))
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Handle edit welcome pic options form submission
   * @param uid the wedding UID
   */
  def doEditWelcomePic(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.WelcomePic.loadWelcomePic(wedding.id).map {
            welcomePic => {
              optionsForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest(views.html.modules.welcomePic.edit(wedding, welcomePic.moduleContent, formWithErrors)),
                form => {
                  models.modules.WelcomePic.updatePictureOptions(welcomePic.moduleContent.copy(showName = form._1, name = form._2))
                  Redirect(controllers.modules.welcomePic.routes.WelcomePic.editWelcomePic(uid)).flashing("success" -> Messages("main.modules.welcomePic.modificationsSaved"))
                }
              )
            }
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle welcome picture upload AJAX form submission
   * @param uid the wedding UID
   */
  def addWelcomePictureAjax(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.WelcomePic.loadWelcomePic(wedding.id).map {
            welcomePic => {
              request.body.asMultipartFormData match {
                case Some(body) => {
                  val responses: Seq[play.api.templates.Html] = body.files.map {
                    uploadFilePart =>
                      val contentType = uploadFilePart.contentType.getOrElse("image/jpeg")
                      val file = uploadFilePart.ref
                      val welcomePicture = models.modules.WelcomePicture(wedding.id, true, Some(UtilsHelper.normalize(uploadFilePart.filename)), Some(contentType), true, None)
                      // The picture should already be resized with JS but this may not work with old browsers. So resize it here again (and compress it a bit more)
                      val resizedFile = getResizedWelcomePicture(welcomePicture, file.file)
                      models.modules.WelcomePic.addPicture(welcomePicture, resizedFile)
                      views.html.modules.welcomePic.welcomePictureEdit(wedding, welcomePicture)
                  }
                  Ok(mkHtml(responses.toList))
                }
                case _ => {
                  BadRequest
                }
              }
            }
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle welcome picture deletion AJAX call
   * @param uid the wedding UID
   */
  def deleteWelcomePictureAjax(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.WelcomePic.loadWelcomePic(wedding.id).map {
            welcomePic => {
              val welcomePictureToDelete = welcomePic.moduleContent.copy(defined = false, filename = None, contentType = None)
              models.modules.WelcomePic.deletePicture(welcomePictureToDelete)
              Ok
            }
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Resize a given welcome picture file to match the normal display expectations (and reduce size before save)
   * @param welcomePicture the welcome picture info
   * @param originalFile the welcome picture file
   * @return a resized welcome picture file
   */
  protected def getResizedWelcomePicture(welcomePicture: models.modules.WelcomePicture, originalFile: File): File = {
    val resizedWelcomePicture = File.createTempFile("resized", null)
    ImagesHelper.resize(welcomePicture.filename.getOrElse("welcomepic.jpg"), originalFile, resizedWelcomePicture, models.modules.WelcomePic.PICTURE_WIDTH, models.modules.WelcomePic.PICTURE_HEIGHT)
    resizedWelcomePicture
  }
}
