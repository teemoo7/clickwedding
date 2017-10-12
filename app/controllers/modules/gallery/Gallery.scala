package controllers.modules.gallery

import controllers.AnyController
import play.api.mvc.Action
import play.api.data.Form
import play.api.data.Forms._
import anorm.{Id, Pk, NotAssigned}
import play.api.i18n.Messages
import java.util.{UUID, Date}
import play.api.{Routes, Play, Logger}
import play.api.libs.iteratee.Enumerator
import app.controllers.helpers.ImagesHelper
import java.io.File
import controllers.helpers.{ModuleHelper, AuthenticationHelper, UtilsHelper}
import play.api.libs.Files

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.03.13
 * Time: 18:43
 * To change this template use File | Settings | File Templates.
 */
object Gallery extends AnyController {

  /**
   * Album creation form definition
   */
  val addAlbumForm = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "title" -> nonEmptyText(maxLength = 50),
      "visibility" -> number.verifying(Messages("main.modules.gallery.error.wrongVisibility"), visibility => ModuleHelper.getGalleryVisibilityValues.keySet.contains(visibility))
    )
      ((id, weddingId, title, visibility) => models.modules.Album(id, weddingId, title, visibility, List()))
      ((album: models.modules.Album) => Some(album.id, album.weddingId, album.title, album.visibility))
  )

  /**
   * Album edition form definition
   */
  val editAlbumForm = Form(
    mapping(
      "id" -> number,
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "title" -> nonEmptyText(maxLength = 50),
      "visibility" -> number.verifying(Messages("main.modules.gallery.error.wrongVisibility"), visibility => ModuleHelper.getGalleryVisibilityValues.keySet.contains(visibility))
    )
      ((id, weddingId, title, visibility) => models.modules.Album(Id(id), weddingId, title, visibility, List()))
      ((album: models.modules.Album) => Some(album.id.get.toInt, album.weddingId, album.title, album.visibility))
  )


  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Gallery.addPictureAjax,
        routes.javascript.Gallery.deletePictureAjax,
        routes.javascript.Gallery.deleteAlbumAjax
      )
    ).as("text/javascript")
  }

  /**
   * Display the gallery page of a given wedding
   * @param uid the wedding UID
   */
  def display(uid: String) = UserAwareAction {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery =>
              val isOwner = AuthenticationHelper.isOwnerOfWedding(wedding.uid)
              val isGuestsWithRestrictedArea = AuthenticationHelper.isGuestWithRestrictedArea(wedding.uid)
              val allowedVisibility = isOwner match {
                case true => models.modules.Gallery.VISIBILITY_OWNER
                case false => isGuestsWithRestrictedArea match {
                  case true => models.modules.Gallery.VISIBILITY_VIP
                  case false => models.modules.Gallery.VISIBILITY_GUESTS
                }
              }
              val galleryInfo = gallery.moduleContent.copy(albums = gallery.moduleContent.albums.filter(a => a.visibility <= allowedVisibility))
              Ok(views.html.modules.gallery.display(wedding, galleryInfo))
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }
  /**
   * Display the admin console of the gallery
   * @param uid the wedding UID
   */
  def editGallery(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery =>
              val albums = models.modules.Gallery.getAlbums(wedding.id)
              Ok(views.html.modules.gallery.editGallery(wedding, gallery, albums, addAlbumForm))
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Handle album creation form submission
   * @param uid the wedding UID
   */
  def addAlbum(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery =>
              addAlbumForm.bindFromRequest.fold(
                formWithErrors => {
                  val albums = models.modules.Gallery.getAlbums(wedding.id)
                  BadRequest(views.html.modules.gallery.editGallery(wedding, gallery, albums, formWithErrors))
                },
                album => {
                  models.modules.Gallery.addAlbum(album.copy(weddingId = wedding.id))
                  goToGalleryEdit(uid).flashing("success" -> Messages("main.modules.gallery.success.albumCreated"))
                }
              )
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Display the edition form of an album
   * @param uid the wedding UID
   * @param id the album ID
   */
  def editAlbum(uid: String, id: Long) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery =>
              models.modules.Gallery.getAlbum(wedding.id, Id(id)).map {
                album =>
                  Ok(views.html.modules.gallery.editAlbum(wedding, gallery, album, editAlbumForm.fill(album)))
              }.getOrElse(NotFound)
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Handle album edition form submission
   * @param uid the wedding UID
   */
  def doEditAlbum(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery =>
              editAlbumForm.bindFromRequest.fold(
                formWithErrors => {
                  models.modules.Gallery.getAlbum(wedding.id, Id(formWithErrors.data("id").toLong)) match {
                    case Some(album) =>
                      BadRequest(views.html.modules.gallery.editAlbum(wedding, gallery, album, formWithErrors))
                    case _ =>
                      NotFound
                  }
                },
                album => {
                  models.modules.Gallery.editAlbum(album)
                  goToAlbumEdit(uid, album.id).flashing("success" -> Messages("main.modules.gallery.success.modificationsSaved"))
                }
              )
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle album deletion AJAX form submission
   * @param uid the wedding UID
   * @param id the album ID
   */
  def deleteAlbumAjax(uid: String, id: Long) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery =>
              models.modules.Gallery.getAlbum(wedding.id, Id(id)).map {
                album =>
                  album.pictures.isEmpty match {
                    case true => {
                      models.modules.Gallery.deleteAlbum(album.id)
                      Ok
                    }
                    case false => {
                      BadRequest(Messages("main.modules.gallery.error.albumNotEmpty"))
                    }
                  }
              }.getOrElse(NotFound)
          }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Handle picture upload AJAX form submission
   * @param uid the wedding UID
   */
  def addPictureAjax(uid: String, id: Long) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Gallery.loadGallery(wedding.id).map {
            gallery => {
              val albumId = Id(id)
              if (gallery.moduleContent.albums.map(a => a.id).contains(albumId)) {
                request.body.asMultipartFormData match {
                  case Some(body) => {
                    val responses: Seq[play.api.templates.Html] = body.files.map {
                      uploadFilePart =>
                        val contentType = uploadFilePart.contentType.getOrElse("image/jpeg")
                        val file = uploadFilePart.ref
                        val picture = models.modules.Picture(null, wedding.id, albumId, uploadFilePart.filename, file.file.length.toInt, new Date(), contentType)
                        // The picture should already be resized with JS but this may not work with old browsers. So resize it here again (and compress it a bit more)
                        val resizedPicture = getResizedPicture(picture, file.file)
                        val thumbnail = getThumbnailPicture(picture, resizedPicture)
                        val pictureWithId = models.modules.Gallery.addPicture(picture.copy(size = resizedPicture.length.toInt), resizedPicture, thumbnail)
                        views.html.modules.gallery.pictureEdit(pictureWithId, id, uid)
                    }
                    Ok(mkHtml(responses.toList))
                  }
                  case _ => {
                    BadRequest
                  }
                }
              } else {
                BadRequest
              }
            }
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle picture deletion AJAX call
   * @param uid the wedding UID
   * @param id the picture ID
   */
  def deletePictureAjax(uid: String, id: Long) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Gallery.getPicture(Id(id)).map {
            picture => {
              picture.weddingId.get == wedding.id.get match {
                case true => {
                  models.modules.Gallery.deletePicture(picture)
                  Ok
                }
                case false => {
                  Unauthorized
                }
              }
            }
          }.getOrElse(NotFound)
        }
      }.getOrElse(NotFound)
  }

  /**
   * Redirect to the edit form of the gallery
   */
  def goToGalleryEdit(uid: String) = Redirect(controllers.modules.gallery.routes.Gallery.editGallery(uid))

  /**
   * Redirect to the edit form of an album
   */
  def goToAlbumEdit(uid: String, id: Pk[Long]) = Redirect(controllers.modules.gallery.routes.Gallery.editAlbum(uid, id.get))

  /**
   * Resize a given picture file to match the normal display expectations (and reduce size before save)
   * @param picture the picture info
   * @param originalFile the picture file
   * @return a resized picture file
   */
  protected def getResizedPicture(picture: models.modules.Picture, originalFile: File): File = {
    val resizedPicture = File.createTempFile("resized", null)
    ImagesHelper.resize(picture.filename, originalFile, resizedPicture, models.modules.Gallery.PICTURE_WIDTH, models.modules.Gallery.PICTURE_HEIGHT)
    resizedPicture
  }

  /**
   * Create a thumbnail of a given picture file
   * @param picture the picture info
   * @param originalFile the picture file
   * @return a thumbnail picture file
   */
  protected def getThumbnailPicture(picture: models.modules.Picture, originalFile: File): File = {
    val thumbnailPicture = File.createTempFile("thumb", null)
    ImagesHelper.resize(picture.filename, originalFile, thumbnailPicture, models.modules.Gallery.THUMBNAIL_PICTURE_WIDTH, models.modules.Gallery.THUMBNAIL_PICTURE_HEIGHT)
    thumbnailPicture
  }
}
