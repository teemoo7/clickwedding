package models.modules

import anorm._
import anorm.SqlParser._
import play.api.Play.current
import play.api.db.DB
import anorm.~
import models.wedding.Wedding
import models.payment.Purchase
import models.authentication.User
import collection.mutable
import java.util.Date
import controllers.modules.gallery.GalleryPlugin
import java.sql.Connection
import controllers.helpers.{ModuleHelper, AuthenticationHelper}
import play.api.i18n.Lang

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 13.11.12
 * Time: 10:44
 * To change this template use File | Settings | File Templates.
 */
abstract class Module(id: Pk[Long], name: String, content: Any, active: Boolean, price: Int, movieUrl: Option[String], displayType: Int, displayColumn: Option[Int], displayPreviousModuleId: Option[Long], accessLevel: Int = Module.DISPLAY_LEVEL_ALL, instructions: Option[String] = None) {
  def init()(implicit connection: Connection)
  def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection)
  def getId: Pk[Long] = id
  def getName: String = name
  def getDisplayPreviousModuleId = displayPreviousModuleId
  def isDisplayTypeSmall = displayType == Module.DISPLAY_TYPE_SMALL_ONLY || displayType == Module.DISPLAY_TYPE_SMALL_AND_LARGE || displayType == Module.DISPLAY_TYPE_LARGE_AND_SMALL
  def isDisplayTypeLarge = displayType == Module.DISPLAY_TYPE_LARGE_ONLY || displayType == Module.DISPLAY_TYPE_LARGE_AND_SMALL || displayType == Module.DISPLAY_TYPE_SMALL_AND_LARGE
  def isDisplayTypeSystem = displayType == Module.DISPLAY_TYPE_SYSTEM
  def isDisplayTypeLargeOnly = displayType == Module.DISPLAY_TYPE_LARGE_ONLY
  def isDisplayTypeSmallOnly = displayType == Module.DISPLAY_TYPE_SMALL_ONLY
  def hasDisplayLink(wedding: models.wedding.Wedding) = ModuleHelper.getDisplayLink(wedding, getId.get.toInt).isDefined
  def getDisplayColumn: Option[Int] = displayColumn
  def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String]
  def getInstructions(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = instructions
  def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html
  def isAuthorized(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang) = {
    accessLevel match {
      case Module.DISPLAY_LEVEL_ALL =>
        true
      case Module.DISPLAY_LEVEL_VIP =>
        AuthenticationHelper.isGuestWithRestrictedArea(wedding.uid)
      case Module.DISPLAY_LEVEL_OWNER =>
        AuthenticationHelper.isOwnerOfWedding(wedding.uid)
      case Module.DISPLAY_LEVEL_ADMIN =>
        AuthenticationHelper.isUserAdmin
    }
  }
  def canBeDisplayedInColumn(columnType: Int, wedding: models.wedding.Wedding) = {
    columnType match {
      case Module.DISPLAY_COLUMN_CENTER => isDisplayTypeLarge
      case Module.DISPLAY_COLUMN_RIGHT => isDisplayTypeSmall
      case Module.DISPLAY_COLUMN_LEFT => isDisplayTypeSmall
      case Module.DISPLAY_COLUMN_MENU => hasDisplayLink(wedding)
    }
  }
}
case class LazyModule(id: Pk[Long], name: String, active: Boolean, price: Int, movieUrl: Option[String], displayColumn: Option[Int], displayPreviousModuleId: Option[Long], instructions: Option[String] = None)
  extends Module(id, name, null, active, price, movieUrl, Module.DISPLAY_TYPE_SYSTEM, displayColumn, displayPreviousModuleId) {
  override def init()(implicit connection: Connection) = {}
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {}
  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = None
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {play.api.templates.Html("")}
}
case class ModulesList(modulesList: List[LazyModule])

object Module {
  val DISPLAY_TYPE_SYSTEM = 0
  val DISPLAY_TYPE_SMALL_ONLY = 1
  val DISPLAY_TYPE_SMALL_AND_LARGE = 2
  val DISPLAY_TYPE_LARGE_AND_SMALL = 3
  val DISPLAY_TYPE_LARGE_ONLY = 4

  val DISPLAY_COLUMN_CENTER = 0
  val DISPLAY_COLUMN_RIGHT = 1
  val DISPLAY_COLUMN_LEFT = 2
  val DISPLAY_COLUMN_MENU = 3
  val DISPLAY_COLUMNS = Array(DISPLAY_COLUMN_CENTER, DISPLAY_COLUMN_RIGHT, DISPLAY_COLUMN_LEFT, DISPLAY_COLUMN_MENU)

  val DISPLAY_LEVEL_ALL = 0
  val DISPLAY_LEVEL_VIP = 1
  val DISPLAY_LEVEL_OWNER = 2
  val DISPLAY_LEVEL_ADMIN = 3


  /**
   * Parse a simple (lazy) module from a ResultSet (lazy means that no content is loaded, only the basic info of the module)
   */
  val module = {
    get[Pk[Long]]("id") ~
      get[String]("name") ~
      get[Option[String]]("movieurl") ~
      get[Int]("price") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") ~
      get[Boolean]("active") ~
      get[Option[String]]("instructions") map {
      case id ~ name ~ movieUrl ~ price ~ displayColumn ~ displayPreviousModuleId ~ active ~ instructions => LazyModule(id, name, active, price, movieUrl, displayColumn, displayPreviousModuleId, instructions)
    }
  }
  /**
   * Parse a simple (lazy) module from a ResultSet with its price (lazy means that no content is loaded, only the basic info of the module)
   */
  val modulePrice = {
    get[Pk[Long]]("id") ~
      get[String]("name") ~
      get[Option[String]]("movieurl") ~
      get[Int]("price") map {
      case id ~ name ~ movieUrl ~ price => LazyModule(id, name, false, price, movieUrl, None, None)
    }
  }

  /**
   * Load the active modules for a wedding
   * @param weddingId
   * @return a list of modules
   */
  def getActiveModulesForWedding(weddingId: Pk[Long]) = {
    DB.withTransaction {
      implicit connection =>
        val list =
        SQL(
          """
          SELECT m.id, m.name, m.movieurl, m.price, wm.active, wm.displaycolumn, wm.displayprevious, wm.instructions FROM weddingmodule wm, module m WHERE
           wm.weddingid = {weddingId} AND
           wm.moduleid = m.id AND
           m.enabled = 1 AND
           wm.active = 1
           ORDER BY wm.displaycolumn, wm.displayprevious
          """
        ).on(
          'weddingId -> weddingId
        ).as(module *)
        loadModulesFromList(list, weddingId)
    }
  }

  /**
   * Load the modules in their implementations from a lazy module list for a wedding
   * @param list
   * @param weddingId
   * @return a list of implemented modules
   */
  protected def loadModulesFromList(list: List[LazyModule], weddingId: Pk[Long])(implicit connection: Connection): List[Module] = {
    for (mod <- list)
      yield mod.id.get.toInt match {
        case MainModule.ID =>
          MainModule(weddingId, mod.id, mod.name, mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)
        case RichText.ID =>
          RichText.loadWithConnection(weddingId).getOrElse(RichText(weddingId, mod.id, mod.name, "", mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)).copy(movieUrl = mod.movieUrl)
        case Contact.ID =>
          Contact.loadWithConnection(weddingId).getOrElse(Contact(weddingId, mod.id, mod.name, ContactInfo(), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId, mod.instructions)).copy(movieUrl = mod.movieUrl)
        case AdFree.ID =>
          AdFree(weddingId, mod.id, mod.name, mod.active, mod.movieUrl)
        case Register.ID =>
          Register.loadWithConnection(weddingId).getOrElse(Register(weddingId, mod.id, mod.name, RegisterInfo(true, true, true, false, None), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId, mod.instructions)).copy(movieUrl = mod.movieUrl)
        case GeoLocal.ID =>
          GeoLocal.loadGeoLocalWithConnection(weddingId).getOrElse(GeoLocal(weddingId, mod.id, mod.name, GeoLocalInfo(GeoLocal.DEFAULT_MAP_TYPE, List()), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)).copy(movieUrl = mod.movieUrl)
//          GeoLocal(weddingId, mod.id, mod.name, GeoLocalInfo(GeoLocal.DEFAULT_MAP_TYPE, List()), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)
          //mod
        case GiftList.ID =>
          GiftList.loadGiftListWithConnection(weddingId).getOrElse(GiftList(weddingId, mod.id, mod.name, GiftListInfo(weddingId, GiftListPayPalInfo(None, models.modules.GiftList.PAYPAL_PERMISSIONS_STATUS_NOT_WORKING, new Date(), None, None, None), None, controllers.modules.GiftList.CURRENCY_CHF, mutable.HashMap()), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId, mod.instructions)).copy(movieUrl = mod.movieUrl)
//          GiftList(weddingId, mod.id, mod.name, GiftListInfo(weddingId, GiftListPayPalInfo(None, models.modules.GiftList.PAYPAL_PERMISSIONS_STATUS_NOT_WORKING, new Date(), None, None, None), None, controllers.modules.GiftList.CURRENCY_CHF, mutable.HashMap()), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId, mod.instructions)
        case Budget.ID =>
          Budget(weddingId, mod.id, mod.name, BudgetInfo(weddingId, controllers.modules.Budget.CURRENCY_CHF, List(), List()), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)
        case Gallery.ID =>
//          Gallery.loadGallery(weddingId).getOrElse(Gallery(weddingId, mod.id, mod.name, GalleryInfo(weddingId, List()), mod.active))
          Gallery(weddingId, mod.id, mod.name, GalleryInfo(weddingId, List()), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)
//          mod
        case WelcomePic.ID =>
          WelcomePic.loadWelcomePicWithConnection(weddingId).getOrElse(WelcomePic(weddingId, mod.id, mod.name, WelcomePicture(weddingId, false, None, None, true, None), mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)).copy(movieUrl = mod.movieUrl)
        case Customize.ID =>
          Customize.loadCustomizationWithConnection(weddingId).getOrElse(Customize(weddingId, mod.id, mod.name, DefaultData(), mod.active, mod.movieUrl)).copy(movieUrl = mod.movieUrl)
        case QRCode.ID =>
          QRCode(weddingId, mod.id, mod.name, mod.active, mod.movieUrl, mod.displayColumn, mod.displayPreviousModuleId)
        case _ =>
          mod
      }
  }

  /**
   * Load the available modules for a wedding
   * @param weddingId
   * @return an object ModulesList that is made of a list of modules
   */
  def getAllModulesForWedding(weddingId: Pk[Long]) = {
    DB.withConnection {
      implicit connection =>
        getAllModulesForWeddingWithConnection(weddingId)
    }
  }

  /**
   * Load the available modules for a wedding
   * @param weddingId
   * @return an object ModulesList that is made of a list of modules
   */
  def getAllModulesForWeddingWithConnection(weddingId: Pk[Long])(implicit connection: Connection) = {
    DB.withConnection {
      implicit connection =>
        val list =
          SQL(
            """
            SELECT m.id, m.name, m.movieurl, m.price, wm.active, wm.displaycolumn, wm.displayprevious, wm.instructions FROM weddingmodule wm, module m WHERE
             wm.weddingid = {weddingId} AND
             wm.moduleid = m.id AND
             m.enabled = 1
             ORDER BY wm.displaycolumn, wm.displayprevious
            """
          ).on(
            'weddingId -> weddingId
          ).as(module *)
        ModulesList(list)
    }
  }

  /**
   * Load all the available modules for a wedding and instance them
   * @param weddingId
   * @return a map of instanciated modules
   */
  def getAllModulesForWeddingAndInstanceThem(weddingId: Pk[Long])(implicit connection: Connection) = {
    loadModulesFromList(getAllModulesForWedding(weddingId).modulesList, weddingId)
  }

  /**
   * Load the modules that have not been added to the wedding yet
   * @param weddingId
   * @return an object ModulesList that is made of a list of modules
   */
  def getBuyableModulesForWedding(weddingId: Pk[Long]) = {
    DB.withConnection {
      implicit connection =>
        val list =
          SQL(
            """
            SELECT m.id, m.name, m.movieurl, m.price FROM module m WHERE
             m.enabled = 1 AND m.id NOT IN (
              SELECT wm.moduleid FROM weddingmodule wm WHERE
               wm.weddingid = {weddingId}
             ) ORDER BY m.price ASC
            """
          ).on(
            'weddingId -> weddingId
          ).as(modulePrice *)
        ModulesList(list)
    }
  }

  /**
   * Activate a module for a wedding
   * @param wedding
   * @param module
   */
  def activateModuleForWedding(wedding: Wedding, module: Module) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE weddingmodule wm
          SET wm.active = 1
          WHERE
           wm.weddingid = {weddingId} AND
           wm.moduleid = {moduleId}
          """
        ).on(
          'weddingId -> wedding.id,
          'moduleId -> module.getId
        ).executeUpdate()
    }
  }

  /**
   * Deactivate a module for a wedding
   * @param wedding
   * @param module
   */
  def deactivateModuleForWedding(wedding: Wedding, module: Module) = {
    DB.withTransaction {
      implicit connection =>
        SQL(
          """
          UPDATE weddingmodule wm
          SET wm.active = 0
          WHERE
           wm.weddingid = {weddingId} AND
           wm.moduleid = {moduleId}
          """
        ).on(
          'weddingId -> wedding.id,
          'moduleId -> module.getId
        ).executeUpdate()
        removeModuleColumnDisplay(wedding, module)
    }
  }

  /**
   * Add a module to a wedding
   * @param weddingId
   * @param moduleId
   */
  def addModuleToWedding(weddingId: Pk[Long], moduleId: Pk[Long]) = {
    DB.withTransaction {
      implicit connection =>
        val result = addModuleToWeddingWithConnection(weddingId, moduleId)
        models.modules.Module.initModuleWithConnection(moduleId.get, weddingId)
        result
    }
  }

  /**
   * Add a module to a wedding given an implicit connection (transaction)
   * @param weddingId
   * @param moduleId
   */
  def addModuleToWeddingWithConnection(weddingId: Pk[Long], moduleId: Pk[Long])(implicit connection: Connection) = {
    SQL(
      """
      INSERT INTO weddingmodule (
        weddingid, moduleid, active, displaycolumn
      ) VALUES (
       {weddingId}, {moduleId}, 1, NULL
      )
      """
    ).on(
      'weddingId -> weddingId,
      'moduleId -> moduleId
    ).executeUpdate()
  }

  /**
   * Buy a module to a wedding in two steps:
   * 1. Update the remaining money of the module
   * 2. Add the module to the wedding
   * @param wedding
   * @param module
   * @param user
   */
  def buyModuleToWedding(wedding: Wedding, module: LazyModule, user: User)(implicit connection: Connection) = {
        models.payment.Money.addPurchase(wedding, user, Purchase(module))
        val result = SQL(
          """
          INSERT INTO weddingmodule (
            weddingid, moduleid, active, displaycolumn
          ) VALUES (
           {weddingId}, {moduleId}, 1, NULL
          )
          """
        ).on(
          'weddingId -> wedding.id,
          'moduleId -> module.id
        ).executeUpdate()
        models.modules.Module.initModuleWithConnection(module.id.get, wedding.id)
        result
  }

  /**
   * Remove a module for a wedding
   * @param weddingId the wedding ID
   * @param module the module
   */
  def deleteModuleForWedding(weddingId: Pk[Long], module: Module) = {
    DB.withTransaction {
      implicit connection =>
        module.deleteWedding(weddingId)
        SQL(
          """
          DELETE FROM weddingmodule
          WHERE
            weddingid = {weddingId} AND
            moduleid = {moduleId}
          """
        ).on(
          'weddingId -> weddingId,
          'moduleId -> module.getId
        ).executeUpdate()
    }
  }

  /**
   * Determine if a given module is free or not
   * @param moduleId the module ID
   * @return true if the module does not cost anything
   */
  def isModuleFree(moduleId: Long) = {
    DB.withConnection {
      implicit connection =>
        val module =
          SQL(
            """
            SELECT m.id, m.name, m.movieurl, m.price FROM module m WHERE
              m.id = {moduleId}
            """
          ).on(
            'moduleId -> moduleId
          ).as(modulePrice.singleOpt)
        module.map {
          mod => mod.price == 0
        }.getOrElse(false)
    }
  }

  /**
   * Return the module
   * @param moduleId the module ID
   * @return the module if exists
   */
  def getModule(moduleId: Long) = {
    DB.withTransaction {
      implicit connection =>
        getModuleWithConnection(moduleId)
    }
  }

  /**
   * Return the module
   * @param moduleId the module ID
   * @return the module if exists
   */
  def getModuleWithConnection(moduleId: Long)(implicit connection: Connection) = {
    SQL(
      """
      SELECT m.id, m.name, m.movieurl, m.price FROM module m WHERE
        m.id = {moduleId} and m.enabled = 1
      """
    ).on(
      'moduleId -> moduleId
    ).as(modulePrice.singleOpt)
  }

  /**
   * Initialize the module for the given wedding
   * @param moduleId the module ID
   * @param weddingId the wedding ID
   */
  def initModuleWithConnection(moduleId: Long, weddingId: Pk[Long])(implicit connection: Connection): Unit = {
    getModuleWithConnection(moduleId) match {
      case Some(module) =>
        loadModulesFromList(List(module), weddingId).find(m => m.getId.get == moduleId) match {
          case Some(fullModule) =>
            fullModule.init
          case None =>
            throw new Exception
        }
      case None =>
        throw new Exception
    }
  }

  /**
   * Update the column in which the modules are displayed
   * @param wedding the wedding
   * @param columnId the column ID in which the modules are displayed now
   * @param modules the modules ID list
   * @return
   */
  def updateModulesColumn(wedding: Wedding, columnId: Int, modules: List[Int]) = {
    DB.withTransaction {
      implicit connection =>
        var lastId: Option[Int] = None
        for (moduleId <- modules) {
          SQL(
            """
            UPDATE weddingmodule
            SET displaycolumn = {columnId}, displayprevious = {lastId}
            WHERE
             weddingid = {weddingId} AND
             moduleid = {moduleId}
            """
          ).on(
            'columnId -> columnId,
            'lastId -> lastId,
            'weddingId -> wedding.id,
            'moduleId -> moduleId
          ).executeUpdate()
          lastId = Some(moduleId)
        }
    }
  }

  /**
   * Reorder the modules when a module is removed (desactivated)
   * @param wedding
   * @param module
   */
  def removeModuleColumnDisplay(wedding: Wedding, module: Module)(implicit connection: Connection) = {
    SQL(
      """
      UPDATE weddingmodule
      SET displayprevious = NULL
      WHERE
       weddingid = {weddingId} AND
       moduleid = {moduleId}
      """
    ).on(
      'weddingId -> wedding.id,
      'moduleId -> module.getId
    ).executeUpdate()
    SQL(
      """
      UPDATE weddingmodule
      SET displayprevious = {displayPrevious}
      WHERE
       weddingid = {weddingId} AND
       displayprevious = {moduleId}
      """
    ).on(
      'displayPrevious -> module.getDisplayPreviousModuleId,
      'weddingId -> wedding.id,
      'moduleId -> module.getId
    ).executeUpdate()
  }

}
