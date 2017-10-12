package controllers.modules

import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import anorm.{Id, Pk, NotAssigned}
import play.api.Routes
import play.api.mvc.Action
import controllers.AnyController

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 12.07.13
 * Time: 17:26
 * To change this template use File | Settings | File Templates.
 */
object Portlet extends AnyController {

  /**
   * Update portlet column order
   */
  val portletsForm = Form(
    tuple(
      "columnId" -> number.verifying(columnId => models.modules.Module.DISPLAY_COLUMNS.contains(columnId)),
      "modules" -> list(number)
    )
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Portlet.editPortlets
      )
    ).as("text/javascript")
  }

  /**
   * Handle AJAX portlet order update
   * @param uid the wedding UID
   */
  def editPortlets(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          portletsForm.bindFromRequest.fold(
            formWithErrors =>
              BadRequest,
            form => {
              val columnId = form._1
              val modules = form._2
              modules.filterNot(moduleId => wedding.modules.find(module => module.getId.get == moduleId).isDefined && wedding.modules.find(module => module.getId.get == moduleId).get.canBeDisplayedInColumn(columnId, wedding)).isEmpty match {
                case true => {
                  models.modules.Module.updateModulesColumn(wedding, columnId, modules)
                  Ok
                }
                case false =>
                  BadRequest
              }
            }
          )
        }
      }.getOrElse(NotFound)
  }
}
