package controllers.modules

import controllers.{AnyController, Wedding}
import play.api.data._
import play.api.data.Forms._
import anorm.Id
import play.api.i18n.Messages
import models.modules.LazyModule
import play.api.Routes
import play.api.mvc.Action
import controllers.helpers.ModuleHelper
import play.api.db.DB
import play.api.Play.current

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 04.12.12
 * Time: 17:34
 * To change this template use File | Settings | File Templates.
 */
object Module extends AnyController {

  /**
   * Modules list form definition
   */
  val modulesListForm: Form[models.modules.ModulesList] = Form(
    mapping(
      "modulesList" -> list(
              mapping(
                "id" -> number,
                "name" -> optional(text(maxLength = 30)),
                "active" -> boolean,
                "movieUrl" -> optional(text),
                "price" -> optional(number)
              )
              (
                (id, name, active, movieUrl, price) => models.modules.LazyModule(Id(id), name.getOrElse(""), active, price.getOrElse(0), None, None, None)
              )
              (
                (module: models.modules.LazyModule) => Some(module.id.get.toInt, Some(module.name), module.active, module.movieUrl, Some(module.price))
              )
      )
    )
    (models.modules.ModulesList.apply)(models.modules.ModulesList.unapply)
  )

  /**
   * Update module status form
   */
  val moduleStatusForm = Form(
    tuple(
      "moduleId" -> number,
      "active" -> boolean
    )
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Module.updateModule
      )
    ).as("text/javascript")
  }

  /**
   * Display the list of all modules and the control to activate/deactivate them
   */
  def listAllModules(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          Ok(views.html.modules.list(
            modulesListForm.fill(models.modules.ModulesList(models.modules.Module.getAllModulesForWedding(wedding.id).modulesList.sortBy(m => Messages("main.modules.name.".concat(m.id.get.toString))))),
            modulesListForm.fill(models.modules.ModulesList(models.modules.Module.getBuyableModulesForWedding(wedding.id).modulesList.sortWith((m1, m2) => m1.price.compareTo(m2.price) < 0))),
            wedding
          ))
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle modules update form submission (activation/deactivation of modules)
   */
  def saveAllModules(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      modulesListForm.bindFromRequest.fold(
        formWithErrors =>
          models.wedding.Wedding.findByUid(uid).map {
            wedding => {
              BadRequest(views.html.modules.list(formWithErrors, modulesListForm.fill(models.modules.Module.getBuyableModulesForWedding(wedding.id)), wedding))
            }
          }.getOrElse(NotFound),
        modulesList => {
          models.wedding.Wedding.findByUid(uid).map {
            wedding => {
              for (module <- modulesList.modulesList) {
                if (module.active) {
                  models.modules.Module.activateModuleForWedding(wedding, module)
                } else {
                  models.modules.Module.deactivateModuleForWedding(wedding, module)
                }
              }
              Wedding.goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.modules.modificationsSaved"))
            }
          }.getOrElse(NotFound)
        }
      )
  }

  /**
   * Remove the given module for the wedding and display the updated module list
   * @param uid the wedding UID
   * @param id the module ID
   */
  def removeModule(uid: String, id: Long) = SecuredAction(IsAdministrator()) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          wedding.modules.find(module => module.getId.get == id) match {
            case Some(module) => {
              val fullModule = module.isInstanceOf[LazyModule] match {
                case true => module
                case false => module
              }
              if (models.modules.Module.deleteModuleForWedding(wedding.id, fullModule) > 0) {
                goToModulesList(uid).flashing("success" -> Messages("main.modules.moduleRemoved"))
              } else {
                goToModulesList(uid).flashing("error" -> Messages("main.modules.errorCouldNotRemoveModule"))
              }
            }
            case None => {
              goToModulesList(uid).flashing("error" -> Messages("main.modules.errorCouldNotRemoveModule"))
            }
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Add the given module for the wedding and display the updated module list
   * @param uid the wedding UID
   * @param id the module ID
   */
  def addModule(uid: String, id: Long) = SecuredAction(IsOwnerOfWeddingAndModuleFree(uid, id)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          if (!wedding.modules.map(_.getId.get).contains(id)) {
            if (models.modules.Module.addModuleToWedding(wedding.id, Id(id)) > 0) {
              ModuleHelper.getEditLink(wedding, id.toInt) match {
                case Some(editLink) =>
                  Redirect(editLink).flashing("success" -> Messages("main.modules.moduleAdded"))
                case None =>
                  Wedding.goToCurrentWedding(uid).flashing("success" -> Messages("main.modules.moduleAdded"))
              }
            } else {
              goToModulesList(uid).flashing("error" -> Messages("main.modules.errorCouldNotAddModule"))
            }
          } else {
            goToModulesList(uid).flashing("error" -> Messages("main.modules.errorCouldNotAddModule"))
          }
        }
      }.getOrElse(NotFound)
  }

   /**
   * Buy the given module for the wedding and display the updated module list
   * @param uid the wedding UID
   * @param id the module ID
   */
  def buyModule(uid: String, id: Long) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      DB.withTransaction {
        implicit connection => {
          models.wedding.Wedding.findByUid(uid).map {
            wedding => {
              models.modules.Module.getModuleWithConnection(id) match {
                case Some(module) => {
                  if (module.price <= wedding.money) {
                    if (models.modules.Module.buyModuleToWedding(wedding, module, user.get) > 0) {
                      ModuleHelper.getEditLink(wedding, id.toInt) match {
                        case Some(editLink) =>
                          Redirect(editLink).flashing("success" -> Messages("main.modules.moduleBought"))
                        case None =>
                          Wedding.goToCurrentWedding(uid).flashing("success" -> Messages("main.modules.moduleBought"))
                      }
                    } else {
                      goToModulesList(uid).flashing("error" -> Messages("main.modules.errorCouldNotAddModule"))
                    }
                  } else {
                    goToModulesList(uid).flashing("error" -> Messages("main.modules.errorNotEnoughMoney"))
                  }
                }
                case None => NotFound
              }
            }
          }.getOrElse(NotFound)
        }
      }
  }

  /**
   * Handle AJAX module status update
   * @param uid the wedding UID
   */
  def updateModule(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          moduleStatusForm.bindFromRequest.fold(
            formWithErrors =>
              BadRequest,
            form => {
              val moduleId = form._1
              val active = form._2
              models.modules.Module.getAllModulesForWedding(wedding.id).modulesList.find(m => m.getId.get.toInt == moduleId) match {
                case Some(module) => {
                  if (active) {
                    models.modules.Module.activateModuleForWedding(wedding, module)
                  } else {
                    models.modules.Module.deactivateModuleForWedding(wedding, module)
                  }
                  Ok
                }
                case None => {
                  BadRequest
                }
              }
            }
          )
        }
      }.getOrElse(NotFound)
  }

  /**
   * Redirect to the display of the modules list of the wedding
   */
  protected def goToModulesList(uid: String) = Redirect(controllers.modules.routes.Module.listAllModules(uid))
}
