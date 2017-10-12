package controllers.modules.customize

import controllers.{Wedding, AnyController}
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.i18n.Messages
import anorm.{Id, Pk, NotAssigned}
import models.modules._
import models.modules.CustomizeThemeAttributesCentral
import anorm.Id
import models.modules.CustomizeThemeAttributesGeneral
import models.modules.CustomizeThemeAttributesMenu
import scala.Some
import models.modules.CustomizeThemeAttributesBackground
import play.api.Routes
import play.api.mvc.Action
import controllers.helpers.UtilsHelper

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 30.01.13
 * Time: 08:42
 * To change this template use File | Settings | File Templates.
 */
object Customize extends AnyController {
  /**
   * Customization form definition
   */
  val customizeForm = Form(
    mapping (
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "customizeInfo" -> mapping (
        "customizedFont" -> mapping (
          "id" -> number.verifying(Messages("main.modules.customize.error.fontDoesNotExist"), id => models.modules.Customize.loadAvailableFonts.map(font => font.id.get.toInt).contains(id))
        ) ((id) => models.modules.CustomizeFont(Id(id), "", "", 1.0)) ((customizeFont: models.modules.CustomizeFont) => Some(customizeFont.id.get.toInt)),
        "customizedTheme" -> mapping (
          "id" -> number.verifying(Messages("main.modules.customize.error.themeDoesNotExist"), id => models.modules.Customize.loadAvailableThemes.map(theme => theme.id.get.toInt).contains(id)),
          "colors" -> optional(mapping (
            "generalAttributes" -> mapping (
              "generalPrimaryBtnBackColor" -> optional(text(minLength = 6, maxLength = 6)),
              "generalPrimaryBtnFontColor" -> optional(text(minLength = 6, maxLength = 6)),
              "generalBtnFontColor" -> optional(text(minLength = 6, maxLength = 6)),
              "generalLinkFontColor" -> optional(text(minLength = 6, maxLength = 6)),
              "generalTextFontColor" -> optional(text(minLength = 6, maxLength = 6))
            )(CustomizeThemeAttributesGeneral.apply)(CustomizeThemeAttributesGeneral.unapply),
            "menuAttributes" -> mapping (
              "menuFontColor" -> optional(text(minLength = 6, maxLength = 6)),
              "menuBackColor" -> optional(text(minLength = 6, maxLength = 6)),
              "menuBackTransparent" -> optional(boolean),
              "menuCornerRadius" -> optional(number(min = 0, max = 20))
            )(CustomizeThemeAttributesMenu.apply)(CustomizeThemeAttributesMenu.unapply),
            "backgroundAttributes" -> mapping (
              "backgroundPicPos" -> optional(number(min = 0, max = 2)),
              "backgroundBackColor" -> optional(text(minLength = 6, maxLength = 6))
            )(
              (backgroundPicPos, backgroundBackColor) => models.modules.CustomizeThemeAttributesBackground(None, backgroundPicPos, backgroundBackColor)
            )(
              (attributesBackground: models.modules.CustomizeThemeAttributesBackground) => Some(attributesBackground.backgroundPicPos, attributesBackground.backgroundBackColor)
            ),
            "centralAttributes" -> mapping (
              "centralBackPicPos" -> optional(number(min = 0, max = 2)),
              "centralBackColorTop" -> optional(text(minLength = 6, maxLength = 6)),
              "centralBackColorBottom" -> optional(text(minLength = 6, maxLength = 6)),
              "centralBackTransparent" -> optional(boolean),
              "centralShadowH" -> optional(number(min = 0, max = 100)),
              "centralShadowV" -> optional(number(min = 0, max = 100)),
              "centralShadowBlur" -> optional(number(min = 0, max = 100)),
              "centralShadowSpread" -> optional(number(min = 0, max = 100)),
              "centralShadowColor" -> optional(text(minLength = 6, maxLength = 6)),
              "centralCornerRadius" -> optional(number(min = 0, max = 100))
            )(
              (centralBackPicPos, centralBackColorTop, centralBackColorBottom, centralBackTransparent, centralShadowH, centralShadowV, centralShadowBlur, centralShadowSpread, centralShadowColor, centralCornerRadius) =>
                models.modules.CustomizeThemeAttributesCentral(None, centralBackPicPos, centralBackColorTop, centralBackColorBottom, centralBackTransparent, centralShadowH, centralShadowV, centralShadowBlur, centralShadowSpread, centralShadowColor, centralCornerRadius)
            )(
              (attributesCentral: models.modules.CustomizeThemeAttributesCentral) =>
                Some(attributesCentral.centralBackPicPos, attributesCentral.centralBackColorTop, attributesCentral.centralBackColorBottom, attributesCentral.centralBackTransparent, attributesCentral.centralShadowH, attributesCentral.centralShadowV, attributesCentral.centralShadowBlur, attributesCentral.centralShadowSpread, attributesCentral.centralShadowColor, attributesCentral.centralCornerRadius)
            ),
            "mainAttributes" -> mapping (
              "mainBackColor" -> optional(text(minLength = 6, maxLength = 6)),
              "mainBackTransparent" -> optional(boolean),
              "mainCornerRadius" -> optional(number(min = 0, max = 100)),
              "mainSeparatorVisible" -> optional(boolean),
              "mainSeparatorColor" -> optional(text(minLength = 6, maxLength = 6))
            )(CustomizeThemeAttributesMain.apply)(CustomizeThemeAttributesMain.unapply)
          ) (models.modules.CustomizeThemeColors.apply)(models.modules.CustomizeThemeColors.unapply))
        ) ((id, colors) => models.modules.CustomizeTheme(Id(id), "", "", false, colors)) ((customizeTheme: models.modules.CustomizeTheme) => Some(customizeTheme.id.get.toInt, customizeTheme.colors))
  ) (models.modules.CustomizeData.apply)(models.modules.CustomizeData.unapply)
    )
      (
        (weddingId, customizeInfo) =>
          models.modules.Customize(weddingId, Id(models.modules.Customize.ID), models.modules.Customize.NAME, customizeInfo, true, None)
      )
      (
        (customize: models.modules.Customize) =>
          Some(customize.weddingId, models.modules.CustomizeData(customize.moduleContent.font, customize.moduleContent.theme))
      )
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Customize.addPictureAjax,
        routes.javascript.Customize.deletePictureAjax
      )
    ).as("text/javascript")
  }

  /**
   * Handle picture upload AJAX form submission
   * @param uid the wedding UID
   */
  def addPictureAjax(uid: String, picType: Int) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(m => m.getId.get == models.modules.Customize.ID).map {
            customize => {
              request.body.asMultipartFormData match {
                case Some(body) => {
                  val responses: Seq[play.api.templates.Html] = body.files.map {
                    uploadFilePart =>
                      val contentType = uploadFilePart.contentType.getOrElse("image/jpeg")
                      val file = uploadFilePart.ref
                      val picture = picType match {
                        case models.modules.Customize.PIC_TYPE_CENTRAL => {
                          models.modules.CustomizePictureCentral(wedding.id, Some(UtilsHelper.normalize(uploadFilePart.filename)), Some(contentType))
                        }
                        case models.modules.Customize.PIC_TYPE_LOGO => {
                          models.modules.CustomizePictureLogo(wedding.id, Some(UtilsHelper.normalize(uploadFilePart.filename)), Some(contentType))
                        }
                        case _ => {
                          models.modules.CustomizePictureBackground(wedding.id, Some(UtilsHelper.normalize(uploadFilePart.filename)), Some(contentType))
                        }
                      }
                      models.modules.Customize.addPicture(picture, file.file)
                      views.html.modules.customize.pictureEdit(picture)
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
   * Handle picture deletion AJAX call
   * @param uid the wedding UID
   */
  def deletePictureAjax(uid: String, picType: Int) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(m => m.getId.get == models.modules.Customize.ID) match {
            case Some(customize: models.modules.Customize) => {
              customize.moduleContent.theme.colors match {
                case Some(colors) => {
                  picType match {
                    case models.modules.Customize.PIC_TYPE_CENTRAL => {
                      colors.centralAttributes.centralBackPic match {
                        case Some(pic: models.modules.CustomizePictureCentral) => {
                          models.modules.Customize.deletePicture(pic.copy(filename = None, contentType = None))
                          Ok
                        }
                        case _ => NotFound
                      }
                    }
//                    case models.modules.Customize.PIC_TYPE_LOGO => {
//                    }
                    case _ => {
                      colors.backgroundAttributes.backgroundPic match {
                        case Some(pic: models.modules.CustomizePictureBackground) => {
                          models.modules.Customize.deletePicture(pic.copy(filename = None, contentType = None))
                          Ok
                        }
                        case _ => NotFound
                      }
                    }
                  }
                }
                case None => NotFound
              }
            }
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Display edit customization form
   * @param uid the wedding UID
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Customize.loadCustomization(wedding.id).map {
            customize => {
              Ok(views.html.modules.customize.edit(wedding, customizeForm.fill(customize), customize.moduleContent.theme.colors))
            }
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle edit customization form submission
   * @param uid the wedding UID
   */
  def doEdit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          customizeForm.bindFromRequest.fold(
            formWithErrors =>
              models.modules.Customize.loadCustomization(wedding.id).map {
                customize => {
                  BadRequest(views.html.modules.customize.edit(wedding, formWithErrors, customize.moduleContent.theme.colors))
                }
              }.getOrElse(NotFound),
            customize => {
              models.modules.Customize.editCustomization(customize.copy(weddingId = wedding.id))
//              Wedding.goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.modules.customize.modificationsSaved"))
              Redirect(controllers.modules.customize.routes.Customize.edit(uid)).flashing("success" -> Messages("main.modules.customize.modificationsSaved"))
            }
          )
        }
      }.getOrElse(NotFound)
  }

}
