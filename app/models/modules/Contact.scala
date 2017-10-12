package models.modules

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import anorm.~
import anorm.Id
import java.sql.Connection
import play.api.i18n.{Lang, Messages}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 11.12.12
 * Time: 15:18
 * To change this template use File | Settings | File Templates.
 */
case class Contact(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: ContactInfo, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long], instructions: Option[String])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SMALL_AND_LARGE, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_CENTER)), displayPreviousModuleId, instructions = instructions) {

  override def init()(implicit connection: Connection) = {
    Contact.add(this)
  }

  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    Contact.deleteModuleForWedding(weddingId, this)
  }

  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] =
    Some(Messages("main.modules.contact.contact"))
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    if (isSmall) {
      views.html.modules.contact.displaySmall(wedding)
    } else {
      views.html.modules.contact.displayLarge(wedding)
    }
  }

}

case class ContactInfo(coupleMail: Option[String] = None, couplePhone: Option[String] = None, coupleAddressName: Option[String] = None, coupleAddressStreet: Option[String] = None, coupleAddressZip: Option[String] = None, coupleAddressPlace: Option[String] = None, organizerMail: Option[String] = None, organizerPhone: Option[String] = None, organizerName: Option[String] = None)

object Contact {
    val ID = 2
    val NAME = "Contact"
    val RECIPIENT_COUPLE = 0
    val RECIPIENT_ORGANIZER = 1

    /**
     * Parse contact info from a ResultSet
     */
    val contact = {
      get[Pk[Long]]("weddingid") ~
        get[Option[String]]("couplemail") ~
        get[Option[String]]("couplephone") ~
        get[Option[String]]("coupleaddressname") ~
        get[Option[String]]("coupleaddressstreet") ~
        get[Option[String]]("coupleaddresszip") ~
        get[Option[String]]("coupleaddressplace") ~
        get[Option[String]]("organizermail") ~
        get[Option[String]]("organizerphone") ~
        get[Option[String]]("organizername") ~
        get[Boolean]("active") ~
        get[Option[Int]]("displaycolumn") ~
        get[Option[Long]]("displayprevious") ~
        get[Option[String]]("instructions") map {
        case weddingId ~ coupleMail ~ couplePhone ~ coupleAddressName ~ coupleAddressStreet ~ coupleAddressZip ~ coupleAddressPlace ~ organizerMail ~ organizerPhone ~ organizerName ~ active ~ displayColumn ~ displayPreviousModuleId ~ instructions
          => Contact(weddingId, Id(Contact.ID), Contact.NAME, ContactInfo(coupleMail, couplePhone, coupleAddressName, coupleAddressStreet, coupleAddressZip, coupleAddressPlace, organizerMail, organizerPhone, organizerName), active, None, displayColumn, displayPreviousModuleId, instructions)
      }
    }

  /**
   * Load the contact info of a wedding
   * @param weddingId the id of the wedding
   * @return the contact info if it exists
   */
  def loadWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[Contact] = {
    SQL(
      """
      SELECT  c.weddingid, c.couplemail, c.couplephone, c.coupleaddressname, c.coupleaddressstreet, c.coupleaddresszip,
              c.coupleaddressplace, c.organizermail, c.organizerphone, c.organizername, wm.active, wm.displaycolumn, wm.displayprevious, wm.instructions
      FROM mod_contact c, weddingmodule wm
      WHERE
        c.weddingid = {weddingid} AND
        wm.weddingid = c.weddingid AND
        wm.moduleid = {moduleid}
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> Contact.ID
    ).as(contact.singleOpt)
  }

  /**
   * Update and save the contact info
   * @param contact the contact info with the new values
   */
  def edit(contact: Contact) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE mod_contact SET
           couplemail = {couplemail}, couplephone = {couplephone}, coupleaddressname = {coupleaddressname}, coupleaddressstreet = {coupleaddressstreet}, coupleaddresszip = {coupleaddresszip},
           coupleaddressplace = {coupleaddressplace}, organizermail = {organizermail}, organizerphone = {organizerphone}, organizername = {organizername}
          WHERE
           weddingid = {weddingid}
          """
        ).on(
          'couplemail -> contact.moduleContent.coupleMail,
          'couplephone -> contact.moduleContent.couplePhone,
          'coupleaddressname -> contact.moduleContent.coupleAddressName,
          'coupleaddressstreet -> contact.moduleContent.coupleAddressStreet,
          'coupleaddresszip -> contact.moduleContent.coupleAddressZip,
          'coupleaddressplace -> contact.moduleContent.coupleAddressPlace,
          'organizermail -> contact.moduleContent.organizerMail,
          'organizerphone -> contact.moduleContent.organizerPhone,
          'organizername -> contact.moduleContent.organizerName,
          'weddingid -> contact.weddingId
        ).executeUpdate()
    }
  }

  /**
   * Insert a contact info for a wedding
   * @param contact the contact info to insert
   */
  def add(contact: Contact)(implicit connection: Connection) = {
    SQL(
      """
        INSERT INTO mod_contact (
          weddingid, couplemail, couplephone, coupleaddressname, coupleaddressstreet, coupleaddresszip,
          coupleaddressplace, organizermail, organizerphone, organizername
        ) VALUES (
          {weddingid}, {couplemail}, {couplephone}, {coupleaddressname}, {coupleaddressstreet}, {coupleaddresszip}, {coupleaddressplace}, {organizermail}, {organizerphone}, {organizername}
        )
      """
    ).on(
      'weddingid -> contact.weddingId,
      'couplemail -> contact.moduleContent.coupleMail,
      'couplephone -> contact.moduleContent.couplePhone,
      'coupleaddressname -> contact.moduleContent.coupleAddressName,
      'coupleaddressstreet -> contact.moduleContent.coupleAddressStreet,
      'coupleaddresszip -> contact.moduleContent.coupleAddressZip,
      'coupleaddressplace -> contact.moduleContent.coupleAddressPlace,
      'organizermail -> contact.moduleContent.organizerMail,
      'organizerphone -> contact.moduleContent.organizerPhone,
      'organizername -> contact.moduleContent.organizerName
    ).executeUpdate()
  }

  /**
   * Update the contact instructions for a given wedding
   * @param contact the contact module
   */
  def editContactInstructions(contact: Contact) = {
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
          'instructions -> contact.instructions,
          'weddingid -> contact.weddingId,
          'moduleid -> contact.moduleId
        ).executeUpdate()
    }
  }

  /**
   * Delete all contact info for the given wedding
   * @param weddingId the wedding ID
   * @param contact the contact info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], contact: Contact)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_contact
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}
