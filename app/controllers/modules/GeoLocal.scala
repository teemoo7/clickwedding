package controllers.modules

import controllers.AnyController
import play.api.mvc.Action
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import anorm.{Id, Pk, NotAssigned}
import play.api.data.format.Formats._
import play.api.Routes


/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 23.01.13
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
object GeoLocal extends AnyController {

  /**
   * Geo localisation edition form definition
   */
  val geoLocalInfoForm: Form[models.modules.GeoLocalInfo] = Form(
    mapping(
      "mapType" -> nonEmptyText(maxLength = 20)
    )
    (
      (mapType) => models.modules.GeoLocalInfo(mapType, List())
    )
    (
      (geoLocalInfo: models.modules.GeoLocalInfo) => Some(geoLocalInfo.mapType)
    )
  )

  /**
   * Marker creation form definition
   */
  val addMarkerForm = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "latitude" -> of[Double],
      "longitude" -> of[Double],
      "zoom" -> number,
      "name" -> nonEmptyText(maxLength = 40),
      "markerType" -> number,
      "address" -> optional(text(maxLength = 40)),
      "isPrivate" -> boolean
    )
      (models.modules.GeoLocalMarker.apply)
      (models.modules.GeoLocalMarker.unapply)
  )

  /**
   * Marker edition form definition
   */
  val editMarkerForm = Form(
    mapping(
      "id" -> number,
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "latitude" -> of[Double],
      "longitude" -> of[Double],
      "zoom" -> number,
      "name" -> nonEmptyText(maxLength = 40),
      "markerType" -> number,
      "address" -> optional(text(maxLength = 40)),
      "isPrivate" -> boolean
    )
      (
        (id, weddingId, latitude, longitude, zoom, name, markerType, address, isPrivate) => models.modules.GeoLocalMarker(Id(id), weddingId, latitude, longitude, zoom, name, markerType, address, isPrivate)
      )
      (
        (geoLocalMarker: models.modules.GeoLocalMarker) => Some(geoLocalMarker.id.get.toInt, geoLocalMarker.weddingId, geoLocalMarker.latitude, geoLocalMarker.longitude, geoLocalMarker.zoom, geoLocalMarker.name, geoLocalMarker.markerType, geoLocalMarker.address, geoLocalMarker.isPrivate)
      )
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.GeoLocal.editGeoLocalInfo,
        routes.javascript.GeoLocal.addMarker,
        routes.javascript.GeoLocal.editMarker,
        routes.javascript.GeoLocal.deleteMarker
      )
    ).as("text/javascript")
  }

  /**
   * Display the geo localization if available
   * @param uid the wedding UID
   */
  def display(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
            case Some(geoLocal: models.modules.GeoLocal) =>
              Ok(views.html.modules.geoLocal.display(wedding, geoLocal.moduleContent))
            case _ => NotFound
          }
      }.getOrElse(NotFound)
  }

  /**
   * Display the admin console for geo localization with the list of markers
   * @param uid the wedding UID
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
              case Some(geoLocal: models.modules.GeoLocal) =>
                Ok(views.html.modules.geoLocal.edit(wedding))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX geo localization info form submission
   * @param uid the wedding UID
   */
  def editGeoLocalInfo(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
              case Some(geoLocal: models.modules.GeoLocal) =>
                geoLocalInfoForm.bindFromRequest.fold(
                  formWithErrors => BadRequest,
                  geoLocalInfo => {
                    models.modules.GeoLocal.editGeoLocal(geoLocal.copy(moduleContent = geoLocalInfo))
                    Ok
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX marker creation form submission
   * @param uid the wedding UID
   */
  def addMarker(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
              case Some(geoLocal: models.modules.GeoLocal) =>
                addMarkerForm.bindFromRequest.fold(
                  formWithErrors => BadRequest,
                  marker => {
                    Ok(views.html.modules.geoLocal.marker(models.modules.GeoLocal.addGeoLocalMarker(marker.copy(weddingId = wedding.id))))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX marker edition form submission
   * @param uid the wedding UID
   */
  def editMarker(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
              case Some(geoLocal: models.modules.GeoLocal) =>
                editMarkerForm.bindFromRequest.fold(
                  formWithErrors => BadRequest,
                  marker => {
                    Ok(views.html.modules.geoLocal.marker(models.modules.GeoLocal.editGeoLocalMarker(marker.copy(weddingId = wedding.id))))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX marker deletion
   * @param uid the wedding UID
   */
  def deleteMarker(uid: String, markerId: Long) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
              case Some(geoLocal: models.modules.GeoLocal) =>
                models.modules.GeoLocal.deleteGeoLocalMarker(Id(markerId))
                Ok
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Load the geo localization module with its markers given a wedding
   * @param wedding the wedding
   * @return the geo localization module if found
   */
  protected def loadGeoLocal(wedding: models.wedding.Wedding): Option[models.modules.GeoLocal] = {
    wedding.modules.find(module => module.getId.get == models.modules.GeoLocal.ID) match {
      case Some(module) =>
        module match {
          case geoLocal: models.modules.GeoLocal =>
            Some(geoLocal)
          case _ =>
            None
        }
      case _ =>
        None
    }
  }
}
