package models.modules

import anorm._
import java.util.Date
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import play.api.i18n.{Lang, Messages}
import com.typesafe.plugin._
import anorm.~
import anorm.Id
import controllers.modules.gallery.GalleryPlugin
import java.io.{InputStream, File}
import java.awt.image.BufferedImage
import java.sql.Connection
import controllers.helpers.AuthenticationHelper

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.03.13
 * Time: 18:44
 * To change this template use File | Settings | File Templates.
 */
case class Gallery(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: GalleryInfo, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_LARGE_ONLY, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_CENTER)), displayPreviousModuleId) {

  override def init()(implicit connection: Connection) = {}

  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    Gallery.deleteModuleForWedding(weddingId, this)
  }

  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] =
    Some(Messages("main.modules.gallery.gallery"))

  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    models.modules.Gallery.loadGallery(wedding.id) match {
      case Some(gallery: models.modules.Gallery) => {
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
        views.html.modules.gallery.displayGallery(wedding, galleryInfo)
      }
      case _ =>
        views.html.modules.notFound(wedding)
    }
  }
}
case class GalleryInfo(weddingId: Pk[Long], albums: List[Album])
case class Album(id: Pk[Long], weddingId: Pk[Long], title: String, visibility: Int, pictures: List[Picture])
case class Picture(id: Pk[Long], weddingId: Pk[Long], albumId: Pk[Long], filename: String, size: Int, date: Date, contentType: String) {
  def key: String = {
    weddingId.get.toString+"/"+albumId.get.toString+"/"+id.get.toString
  }
  def thumbKey: String = {
    weddingId.get.toString+"/"+albumId.get.toString+"/thumb/"+id.get.toString
  }
}

object Gallery {
  val ID = 8
  val NAME = "Gallery"

  val VISIBILITY_GUESTS = 0
  val VISIBILITY_VIP = 1
  val VISIBILITY_OWNER = 2

  val PICTURE_WIDTH = 1024
  val PICTURE_HEIGHT = 768
  val THUMBNAIL_PICTURE_WIDTH = 160
  val THUMBNAIL_PICTURE_HEIGHT = 120

  val MAX_AVAILABLE_SPACE = 1024*1024*10 // 10 MB

  /**
   * Images store definition
   */
  val store = use[GalleryPlugin]

  /**
   * Parse a gallery info from a ResultSet
   */
  val gallery = {
    get[Pk[Long]]("weddingid") ~
      get[Boolean]("active") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") map {
      case weddingId ~ active ~ displayColumn ~ displayPreviousModuleId
      => Gallery(weddingId, Id(Gallery.ID), Gallery.NAME, GalleryInfo(weddingId, getAlbums(weddingId)), active, None, displayColumn, displayPreviousModuleId)
    }
  }

  /**
   * Parse an album from a ResultSet
   */
  val album = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingid") ~
      get[String]("title") ~
      get[Int]("visibility") map {
      case id ~ weddingId ~ title ~ visibility
      => Album(id, weddingId, title, visibility, getPictures(id))
    }
  }

  /**
   * Parse a picture from a ResultSet
   */
  val picture = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingId") ~
      get[Pk[Long]]("albumId") ~
      get[String]("filename") ~
      get[Int]("size") ~
      get[Date]("date") ~
      get[String]("contenttype") map {
      case id ~ weddingId ~ albumId ~ filename ~ size ~ date ~ contentType
      => Picture(id, weddingId, albumId, filename, size, date, contentType)
    }
  }

  /**
   * Load the gallery of a wedding
   * @param weddingId the ID of the wedding
   * @return the gallery if it exists
   */
  def loadGallery(weddingId: Pk[Long]): Option[Gallery] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT wm.weddingid, wm.active, wm.displaycolumn, wm.displayprevious
        FROM weddingmodule wm
        WHERE
          wm.weddingid = {weddingid} AND
          wm.moduleid = {moduleid}
        """
      ).on(
        'weddingid -> weddingId,
        'moduleid -> Gallery.ID
      ).as(gallery.singleOpt)
    }
  }

  /**
   * Load the albums for a given wedding
   * @param weddingId the wedding ID
   * @return a list of albums
   */
  def getAlbums(weddingId: Pk[Long]): List[Album] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, title, visibility
        FROM mod_gallery_album
        WHERE weddingid = {weddingid}
        """
      ).on(
        'weddingid -> weddingId
      ).as(album*)
    }
  }

  /**
   * Load an album for a given wedding
   * @param weddingId the wedding ID
   * @param albumId the album ID
   * @return the album if found
   */
  def getAlbum(weddingId: Pk[Long], albumId: Pk[Long]): Option[Album] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, title, visibility
        FROM mod_gallery_album
        WHERE
          weddingid = {weddingid} AND
          id = {id}
        """
      ).on(
        'weddingid -> weddingId,
        'id -> albumId
      ).as(album.singleOpt)
    }
  }

  /**
   * Insert a new album for the given wedding
   * @param album the initial value for the album
   */
  def addAlbum(album: Album) = {
    DB.withConnection {
      implicit connection =>
        val id: Option[Long] = SQL(
          """
            INSERT INTO mod_gallery_album (
              weddingid, title, visibility
            ) VALUES (
              {weddingid}, {title}, {visibility}
            )
          """
        ).on(
          'weddingid -> album.weddingId,
          'title -> album.title,
          'visibility -> album.visibility
        ).executeInsert()
        album.copy(id = Id(id.get))
    }
  }

  /**
   * Update the album info for a given wedding
   * @param album the album
   */
  def editAlbum(album: Album) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_gallery_album SET
              title = {title}, visibility = {visibility}
            WHERE
              id = {id}
          """
        ).on(
          'title -> album.title,
          'visibility -> album.visibility,
          'id -> album.id
        ).executeUpdate()
    }
  }

  /**
   * Delete an album
   * @param id the id of the album
   */
  def deleteAlbum(id: Pk[Long]) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          DELETE FROM mod_gallery_album
          WHERE
           id = {id}
          """
        ).on(
          'id -> id
        ).executeUpdate()
    }
  }

  /**
   * Insert a new picture
   * @param picture the initial value for the picture
   */
  def addPicture(picture: Picture, file: File, thumb: File) = {
    DB.withTransaction {
      implicit connection =>
        val id: Option[Long] = SQL(
          """
            INSERT INTO mod_gallery_picture (
              weddingid, albumid, filename, size, date, contenttype
            ) VALUES (
              {weddingid}, {albumid}, {filename}, {size}, {date}, {contenttype}
            )
          """
        ).on(
          'weddingid -> picture.weddingId,
          'albumid -> picture.albumId,
          'filename -> picture.filename,
          'size -> picture.size,
          'date -> picture.date,
          'contenttype -> picture.contentType
        ).executeInsert()
      val savedPicture = picture.copy(id = Id(id.get))
      store.savePicture(savedPicture, file, thumb)
      savedPicture
    }
  }

  /**
   * Delete a picture
   * @param picture the picture to be deleted
   */
  def deletePicture(picture: Picture) = {
    DB.withTransaction {
      implicit connection =>
        store.deletePicture(picture)
        SQL(
          """
          DELETE FROM mod_gallery_picture
          WHERE
           id = {id}
          """
        ).on(
          'id -> picture.id
        ).executeUpdate()
    }
  }

  /**
   * Load the pictures of an album
   * @param albumId the album ID
   * @return a list of pictures
   */
  def getPictures(albumId: Pk[Long]): List[Picture] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, albumid, filename, size, date, contenttype
        FROM mod_gallery_picture
        WHERE albumid = {albumid}
        """
      ).on(
        'albumid -> albumId
      ).as(picture*)
    }
  }

  /**
   * Load a picture given its ID
   * @param pictureId the picture ID
   * @return the picture if found
   */
  def getPicture(pictureId: Pk[Long]): Option[Picture] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, albumid, filename, size, date, contenttype
        FROM mod_gallery_picture
        WHERE id = {id}
        """
      ).on(
        'id -> pictureId
      ).as(picture.singleOpt)
    }
  }

  /**
   * Load the picture binary file from the store
   * @param picture the picture to load
   * @return a file of the corresponding picture if found
   */
  def loadPictureFile(picture: Picture): InputStream = {
    store.getPicture(picture)
  }

  /**
   * Return the pre signed picture link for direct download (but with expiration date)
   * @param picture the picture to link
   * @return a string with the URL of the picture direct download
   */
  def getPictureLink(picture: Picture): Option[String] = {
    store.getPictureLink(picture)
  }

  /**
   * Return the pre signed picture thumbnail link for display (but with expiration date)
   * @param picture the picture thumbnail to link
   * @return a string with the URL of the picture thumbnail direct download
   */
  def getPictureThumbnailLink(picture: Picture): Option[String] = {
    store.getPictureThumbnailLink(picture)
  }

  /**
   * Delete all galleries for the given wedding
   * @param weddingId the wedding ID
   * @param gallery the gallery info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], gallery: Gallery)(implicit connection: Connection) = {
    for (album <- getAlbums(weddingId)) {
      for (picture <- album.pictures) {
        store.deletePicture(picture)
      }
      SQL(
        """
        DELETE FROM mod_gallery_picture
        WHERE
        weddingid = {weddingId}
        """
      ).on(
        'weddingId -> weddingId
      ).executeUpdate()
    }
    SQL(
      """
        DELETE FROM mod_gallery_album
        WHERE
        weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}