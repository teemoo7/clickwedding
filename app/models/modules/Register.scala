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
 * Date: 21.01.13
 * Time: 14:38
 * To change this template use File | Settings | File Templates.
 */
case class Register(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: RegisterInfo, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long], instructions: Option[String])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SMALL_AND_LARGE, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_RIGHT)), displayPreviousModuleId, instructions = instructions) {
  override def init()(implicit connection: Connection) = {
    Register.addRegister(this)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    Register.deleteModuleForWedding(weddingId, this)
  }
  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] =
    Some(Messages("main.modules.register.registration"))

  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    wedding.modules.find(module => module.getId.get == models.modules.Register.ID) match {
      case Some(module) =>
        module match {
          case register: models.modules.Register =>
            views.html.modules.register.display(wedding, register.moduleContent, Register.getRegisteredGuests(wedding.id))
        }
      case _ =>
        views.html.modules.notFound(wedding)
    }
  }
}
case class RegisterInfo(isReception: Boolean, isDinner: Boolean, isDinnerPrivate: Boolean, isNotification: Boolean, notificationMail: Option[String])
case class RegisterGuest(weddingId: Pk[Long] = NotAssigned, firstName: Option[String], lastName: String, isComing: Boolean, nbForReception: Int, nbForDinner: Int, mailAddress: Option[String], mobilePhone: Option[String], comment: Option[String])

object Register {
  val ID = 4
  val NAME = "Register"

  /**
   * Parse registration info from a ResultSet
   */
  val register = {
    get[Pk[Long]]("weddingid") ~
      get[Boolean]("reception") ~
      get[Boolean]("dinner") ~
      get[Boolean]("dinnerprivate") ~
      get[Option[String]]("mailnotification") ~
      get[Boolean]("active") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") ~
      get[Option[String]]("instructions") map {
      case weddingId ~ isReception ~ isDinner ~ isDinnerPrivate ~ mailNotification ~ active ~ displayColumn ~ displayPreviousModuleId ~ instructions
        => Register(weddingId, Id(Register.ID), Register.NAME, RegisterInfo(isReception, isDinner, isDinnerPrivate, mailNotification.isDefined, mailNotification), active, None, displayColumn, displayPreviousModuleId, instructions)
    }
  }

  /**
   * Parse registered guests info from a ResultSet
   */
  val registeredGuest = {
    get[Pk[Long]]("weddingid") ~
      get[Option[String]]("firstname") ~
      get[String]("lastname") ~
      get[Int]("nbforreception") ~
      get[Int]("nbfordinner") ~
      get[Option[String]]("email") ~
      get[Option[String]]("mobile") ~
      get[Option[String]]("comment") map {
      case weddingId ~ firstName ~ lastName ~ nbForReception ~ nbForDinner ~ mailAddress ~ mobilePhone ~ comment
        => RegisterGuest(weddingId, firstName, lastName, (nbForReception+nbForDinner) > 0, nbForReception, nbForDinner, mailAddress, mobilePhone, comment)
    }
  }

  /**
   * Insert a new registration info for the given wedding
   * @param register the initial value for the registration
   */
  def addRegister(register: Register)(implicit connection: Connection) = {
    SQL(
      """
        INSERT INTO mod_register (
          weddingid, reception, dinner, dinnerprivate, mailnotification
        ) VALUES (
          {weddingid}, {reception}, {dinner}, {dinnerprivate}, {mailnotification}
        )
      """
    ).on(
      'weddingid -> register.weddingId,
      'reception -> register.moduleContent.isReception,
      'dinner -> register.moduleContent.isDinner,
      'dinnerprivate -> register.moduleContent.isDinnerPrivate,
      'mailnotification -> register.moduleContent.notificationMail
    ).executeUpdate()
  }

  /**
   * Update the registration info for a given wedding
   * @param register the registration info
   */
  def editRegisterInfo(register: Register) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_register SET
              reception = {reception}, dinner = {dinner}, dinnerprivate = {dinnerprivate}, mailnotification = {mailnotification}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> register.weddingId,
          'reception -> register.moduleContent.isReception,
          'dinner -> register.moduleContent.isDinner,
          'dinnerprivate -> register.moduleContent.isDinnerPrivate,
          'mailnotification -> register.moduleContent.notificationMail
        ).executeUpdate()
    }
  }

  /**
   * Update the registration instructions for a given wedding
   * @param register the registration module
   */
  def editRegisterInstructions(register: Register) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE weddingmodule SET
              instructions = {instructions}
            WHERE
              weddingid = {weddingid} AND
              moduleid = {moduleid}
          """
        ).on(
          'instructions -> register.instructions,
          'weddingid -> register.weddingId,
          'moduleid -> register.moduleId
        ).executeUpdate()
    }
  }

  /**
   * Load the registration of a wedding
   * @param weddingId the ID of the wedding
   * @return the registration if it exists
   */
  def load(weddingId: Pk[Long]): Option[Register] = {
    DB.withConnection { implicit connection =>
      loadWithConnection(weddingId)
    }
  }

  /**
   * Load the registration of a wedding
   * @param weddingId the ID of the wedding
   * @return the registration if it exists
   */
  def loadWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[Register] = {
    SQL(
      """
      SELECT  r.weddingid, r.reception, r.dinner, r.dinnerprivate, r.mailnotification, wm.active, wm.displaycolumn, wm.displayprevious, wm.instructions
      FROM mod_register r, weddingmodule wm
      WHERE
        r.weddingid = {weddingid} AND
        wm.weddingid = r.weddingid AND
        wm.moduleid = {moduleid}
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> Register.ID
    ).as(register.singleOpt)
  }


  /**
   * Insert a new guest registration for the given wedding
   * @param registerGuest the value for the new guest registration
   */
  def addGuestRegistration(registerGuest: RegisterGuest) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            INSERT INTO mod_register_guests (
              weddingid, firstname, lastname, nbforreception, nbfordinner, email, mobile, comment
            ) VALUES (
              {weddingid}, {firstname}, {lastname}, {nbforreception}, {nbfordinner}, {email}, {mobile}, {comment}
            )
          """
        ).on(
          'weddingid -> registerGuest.weddingId,
          'firstname -> registerGuest.firstName,
          'lastname -> registerGuest.lastName,
          'nbforreception -> registerGuest.nbForReception,
          'nbfordinner -> registerGuest.nbForDinner,
          'email -> registerGuest.mailAddress,
          'mobile -> registerGuest.mobilePhone,
          'comment -> registerGuest.comment
        ).executeUpdate()
    }
  }

  /**
   * Return the list of all registered guests for the wedding
   * @param weddingId the ID of the wedding
   * @return a list of guests
   */
  def getRegisteredGuests(weddingId: Pk[Long]): List[RegisterGuest] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT weddingid, firstname, lastname, nbforreception, nbfordinner, email, mobile, comment
        FROM mod_register_guests
        WHERE
          weddingid = {weddingid}
        """
      ).on(
        'weddingid -> weddingId
      ).as(registeredGuest *)
    }
  }

  /**
   * Delete all registration info for the given wedding
   * @param weddingId the wedding ID
   * @param register the registration info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], register: Register)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_register_guests
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_register
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}