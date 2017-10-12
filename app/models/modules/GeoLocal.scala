package models.modules

import anorm._
import play.api.db.DB
import play.api.Play.current
import anorm.SqlParser._
import anorm.~
import anorm.Id
import java.sql.Connection
import play.api.i18n.{Lang, Messages}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 23.01.13
 * Time: 16:15
 * To change this template use File | Settings | File Templates.
 */
case class GeoLocal(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: GeoLocalInfo, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SMALL_AND_LARGE, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_CENTER)), displayPreviousModuleId) {
  override def init()(implicit connection: Connection) = {
    GeoLocal.addGeoLocal(this)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    GeoLocal.deleteModuleForWedding(weddingId, this)
  }

  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] =
    Some(Messages("main.modules.geoLocal.geoLocalization"))

  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
      case Some(geoLocal: models.modules.GeoLocal) =>
        views.html.modules.geoLocal.map(wedding, geoLocal.moduleContent)
      case _ =>
        views.html.modules.notFound(wedding)
    }
  }
}

case class GeoLocalInfo(mapType: String, markers: List[GeoLocalMarker])
case class GeoLocalMarker(id: Pk[Long], weddingId: Pk[Long], latitude: Double, longitude: Double, zoom: Int, name: String, markerType: Int, address: Option[String], isPrivate: Boolean)

object GeoLocal {
  val ID = 5
  val NAME = "GeoLocal"
  val DEFAULT_MAP_TYPE = "roadmap"

  /**
   * Parse geo localization info from a ResultSet
   */
  val geoLocal = {
    get[Pk[Long]]("weddingid") ~
      get[String]("maptype") ~
      get[Boolean]("active") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") map {
      case weddingId ~ mapType ~ active ~ displaycolumn ~ displayPreviousModuleId
      => GeoLocal(weddingId, Id(GeoLocal.ID), GeoLocal.NAME, GeoLocalInfo(mapType, getGeoLocalMarkers(weddingId)), active, None, displaycolumn, displayPreviousModuleId)
    }
  }

  /**
   * Parse registered guests info from a ResultSet
   */
  val geoLocalMarker = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingid") ~
      get[Double]("latitude") ~
      get[Double]("longitude") ~
      get[Int]("zoom") ~
      get[String]("name") ~
      get[Int]("markertype") ~
      get[Option[String]]("address") ~
      get[Boolean]("private") map {
      case id ~ weddingId ~ latitude ~ longitude ~ zoom ~ name ~ markerType ~ address ~ isPrivate
        => GeoLocalMarker(id, weddingId, latitude, longitude, zoom, name, markerType, address, isPrivate)
    }
  }

  /**
   * Insert a new geo localization for the given wedding
   * @param geoLocal the initial value for the geo localization
   */
  def addGeoLocal(geoLocal: GeoLocal)(implicit connection: Connection) = {
    SQL(
    """
      INSERT INTO mod_geolocal (
        weddingid, maptype
      ) VALUES (
        {weddingid}, {maptype}
      )
    """
    ).on(
      'weddingid -> geoLocal.weddingId,
      'maptype -> geoLocal.moduleContent.mapType
    ).executeUpdate()
  }

  /**
   * Update the geo localization info for a given wedding
   * @param geoLocal the geo localization info
   */
  def editGeoLocal(geoLocal: GeoLocal) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_geolocal SET
              maptype = {maptype}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> geoLocal.weddingId,
          'maptype -> geoLocal.moduleContent.mapType
        ).executeUpdate()
    }
  }


  /**
   * Load the geo localization info of a wedding
   * @param weddingId the ID of the wedding
   * @return the geo localization if it exists
   */
  def loadGeoLocalWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[GeoLocal] = {
    SQL(
      """
      SELECT g.weddingid, g.maptype, wm.active, wm.displaycolumn, wm.displayprevious
      FROM mod_geolocal g, weddingmodule wm
      WHERE
        g.weddingid = {weddingid} AND
        wm.weddingid = g.weddingid AND
        wm.moduleid = {moduleid}
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> GeoLocal.ID
    ).as(geoLocal.singleOpt)
  }

  /**
   * Insert a new geo localization marker for the given wedding
   * @param geoLocalMarker the value for the new marker
   */
  def addGeoLocalMarker(geoLocalMarker: GeoLocalMarker) = {
    DB.withConnection { implicit connection =>
      val id: Option[Long] = SQL(
        """
          INSERT INTO mod_geolocal_marker (
            weddingid, latitude, longitude, zoom, name, markertype, address, private
          ) VALUES (
            {weddingid}, {latitude}, {longitude}, {zoom}, {name}, {markertype}, {address}, {private}
          )
        """
      ).on(
        'weddingid -> geoLocalMarker.weddingId,
        'latitude -> geoLocalMarker.latitude,
        'longitude -> geoLocalMarker.longitude,
        'zoom -> geoLocalMarker.zoom,
        'name -> geoLocalMarker.name,
        'markertype -> geoLocalMarker.markerType,
        'address -> geoLocalMarker.address,
        'private -> geoLocalMarker.isPrivate
      ).executeInsert()

      geoLocalMarker.copy(id = Id(id.get))
    }
  }

  /**
   * Return the list of all registered guests for the wedding
   * @param weddingId the ID of the wedding
   * @return a list of guests
   */
  def getGeoLocalMarkers(weddingId: Pk[Long]): List[GeoLocalMarker] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, latitude, longitude, zoom, name, markertype, address, private
        FROM mod_geolocal_marker
        WHERE
          weddingid = {weddingid}
        """
      ).on(
        'weddingid -> weddingId
      ).as(geoLocalMarker *)
    }
  }

  /**
   * Update the geo localization marker
   * @param geoLocalMarker the geo localization marker
   */
  def editGeoLocalMarker(geoLocalMarker: GeoLocalMarker) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          UPDATE mod_geolocal_marker SET
            latitude = {latitude}, longitude = {longitude}, zoom = {zoom}, name = {name},
            markertype = {markertype}, address = {address}, private = {private}
          WHERE
            id = {id}
        """
      ).on(
        'latitude -> geoLocalMarker.latitude,
        'longitude -> geoLocalMarker.longitude,
        'zoom -> geoLocalMarker.zoom,
        'name -> geoLocalMarker.name,
        'markertype -> geoLocalMarker.markerType,
        'address -> geoLocalMarker.address,
        'private -> geoLocalMarker.isPrivate,
        'id -> geoLocalMarker.id
      ).executeUpdate()
      geoLocalMarker
    }
  }

  /**
   * Delete a geo localization marker
   * @param id the id of the marker
   */
  def deleteGeoLocalMarker(id: Pk[Long]) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        DELETE FROM mod_geolocal_marker
        WHERE
         id = {id}
        """
      ).on(
        'id -> id
      ).executeUpdate()
    }
  }

  /**
   * Delete all geo localization info for the given wedding
   * @param weddingId the wedding ID
   * @param geoLocal the geo localization info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], geoLocal: GeoLocal)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_geolocal_marker
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_geolocal
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}