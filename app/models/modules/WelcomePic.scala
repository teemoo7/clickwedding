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
import java.io.{InputStream, File}
import java.awt.image.BufferedImage
import controllers.modules.welcomePic.WelcomePicPlugin
import java.sql.Connection

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 17.04.13
 * Time: 11:39
 * To change this template use File | Settings | File Templates.
 */
case class WelcomePic(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: WelcomePicture, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_LARGE_ONLY, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_CENTER)), displayPreviousModuleId) {
  override def init()(implicit connection: Connection) = {
    WelcomePic.addEmptyPic(weddingId)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    WelcomePic.deleteModuleForWedding(weddingId, this)
  }
  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = None
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang):play.api.templates.Html = {
    wedding.modules.find(module => module.getId.get == models.modules.WelcomePic.ID) match {
      case Some(module) =>
        module match {
          case welcomePic: models.modules.WelcomePic =>
            views.html.modules.welcomePic.welcomePicture(wedding, welcomePic.moduleContent)
        }
      case _ =>
        views.html.modules.notFound(wedding)
    }
  }
}
case class WelcomePicture(weddingId: Pk[Long], defined: Boolean, filename: Option[String], contentType: Option[String], showName: Boolean, name: Option[String]) {
  def key: String = {
    weddingId.get.toString
  }
}

object WelcomePic {
  val ID = 9
  val NAME = "WelcomePic"

  val PICTURE_WIDTH = 450
  val PICTURE_HEIGHT = 450

  /**
   * Images store definition
   */
  val store = use[WelcomePicPlugin]

  /**
   * Parse a welcome picture info from a ResultSet
   */
  val welcomePic = {
    get[Pk[Long]]("weddingid") ~
      get[Boolean]("active") ~
      get[Boolean]("defined") ~
      get[Option[String]]("filename") ~
      get[Option[String]]("contenttype") ~
      get[Boolean]("showname") ~
      get[Option[String]]("name") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") map {
      case weddingId ~ active ~ defined ~ filename ~ contentType ~ showName ~ name ~ displayColumn ~ displayPreviousModuleId
      => WelcomePic(weddingId, Id(WelcomePic.ID), WelcomePic.NAME, WelcomePicture(weddingId, defined, filename, contentType, showName, name), active, None, displayColumn, displayPreviousModuleId)
    }
  }

  /**
   * Insert an empty welcome picture for the given wedding
   * @param weddingId the wedding ID
   */
  def addEmptyPic(weddingId: Pk[Long])(implicit connection: Connection) = {
    SQL(
      """
        INSERT INTO mod_welcomepic (
          weddingid, defined, showname
        ) VALUES (
          {weddingid}, {defined}, {showname}
        )
      """
    ).on(
      'weddingid -> weddingId,
      'defined -> false,
      'showname -> false
    ).executeUpdate()
  }

  /**
   * Load the welcome picture of a wedding
   * @param weddingId the ID of the wedding
   * @return the welcome picture if it exists
   */
  def loadWelcomePic(weddingId: Pk[Long]): Option[WelcomePic] = {
    DB.withConnection { implicit connection =>
      loadWelcomePicWithConnection(weddingId)
    }
  }

  /**
   * Load the welcome picture of a wedding
   * @param weddingId the ID of the wedding
   * @return the welcome picture if it exists
   */
  def loadWelcomePicWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[WelcomePic] = {
    SQL(
      """
      SELECT wm.weddingid, wm.active, wp.defined, wp.filename, wp.contenttype, wp.showname, wp.name, wm.displaycolumn, wm.displayprevious
      FROM weddingmodule wm, mod_welcomepic wp
      WHERE
        wm.weddingid = {weddingid} AND
        wm.moduleid = {moduleid} AND
        wp.weddingid = wm.weddingid
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> WelcomePic.ID
    ).as(welcomePic.singleOpt)
  }

  /**
   * Insert a new welcome picture
   * @param welcomePicture the picture info
   * @param file the picture file
   */
  def addPicture(welcomePicture: WelcomePicture, file: File) = {
    DB.withTransaction {
      implicit connection =>
        store.saveWelcomePicture(welcomePicture, file)
        updatePicture(welcomePicture)
    }
  }

  /**
   * Delete the welcome picture
   * @param welcomePicture the picture info
   */
  def deletePicture(welcomePicture: WelcomePicture) = {
    DB.withTransaction {
      implicit connection =>
        store.deleteWelcomePicture(welcomePicture)
        updatePicture(welcomePicture)
    }
  }

  protected def updatePicture(welcomePicture: WelcomePicture)(implicit connection: Connection) = {
    SQL(
      """
      UPDATE mod_welcomepic SET
        defined = {defined},
        filename = {filename},
        contenttype = {contenttype}
      WHERE
        weddingid = {weddingid}
      """
    ).on(
      'defined -> welcomePicture.defined,
      'filename -> welcomePicture.filename,
      'contenttype -> welcomePicture.contentType,
      'weddingid -> welcomePicture.weddingId
    ).executeUpdate()
  }

  /**
   * Update the welcome picture options (not the picture itself)
   * @param welcomePicture the picture info
   */
  def updatePictureOptions(welcomePicture: WelcomePicture) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE mod_welcomepic SET
            showname = {showname},
            name = {name}
          WHERE
            weddingid = {weddingid}
          """
        ).on(
          'showname -> welcomePicture.showName,
          'name -> welcomePicture.name,
          'weddingid -> welcomePicture.weddingId
        ).executeUpdate()
    }
  }

  /**
   * Return the pre signed welcome picture link for direct download (but with expiration date)
   * @param welcomePicture the welcome picture to link
   * @return a string with the URL of the welcome picture direct download
   */
  def getWelcomePictureLink(welcomePicture: WelcomePicture): Option[String] = {
    store.getWelcomePictureLink(welcomePicture)
  }

  /**
   * Delete welcome picture for the given wedding
   * @param weddingId the wedding ID
   * @param welcomePic the picture info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], welcomePic: WelcomePic)(implicit connection: Connection) = {
    store.deleteWelcomePicture(welcomePic.moduleContent)
    SQL(
      """
      DELETE FROM mod_welcomepic
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}