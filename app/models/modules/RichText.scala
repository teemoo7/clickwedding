package models.modules

import anorm._
import anorm.SqlParser._
import play.api.Play.current
import play.api.db.DB
import anorm.~
import java.sql.Connection
import play.api.i18n.Lang

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 06.11.12
 * Time: 15:29
 * To change this template use File | Settings | File Templates.
 */
case class RichText(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: String, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_LARGE_ONLY, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_CENTER)), displayPreviousModuleId) {
  override def init()(implicit connection: Connection) = {
    RichText.add(this)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    RichText.deleteModuleForWedding(weddingId, this)
  }
  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = None
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    wedding.modules.find(module => module.getId.get == models.modules.RichText.ID) match {
      case Some(module) =>
        module match {
          case richText: models.modules.RichText =>
            views.html.modules.richText.display(wedding, richText)
        }
      case _ =>
        views.html.modules.notFound(wedding)
    }
  }
}

object RichText {
  val ID = 1
  val NAME = "RichText"
  /**
   * Parse a rich text from a ResultSet
   */
  val richText = {
    get[Pk[Long]]("weddingid") ~
      get[String]("content") ~
      get[Boolean]("active") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") map {
      case weddingId ~ content ~ active ~ displayColumn ~ displayPreviousModuleId => RichText(weddingId, Id(RichText.ID), RichText.NAME, content, active, None, displayColumn, displayPreviousModuleId)
    }
  }

  /**
   * Load the rich text of a wedding
   * @param weddingId the id of the wedding
   * @return the rich text if it exists
   */
  def load(weddingId: Pk[Long]): Option[RichText] = {
    DB.withConnection { implicit connection =>
      loadWithConnection(weddingId)
    }
  }

  /**
   * Load the rich text of a wedding
   * @param weddingId the id of the wedding
   * @return the rich text if it exists
   */
  def loadWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[RichText] = {
    SQL(
      """
      SELECT mod_richtext.weddingid, mod_richtext.content, weddingmodule.active, weddingmodule.displaycolumn, weddingmodule.displayprevious FROM mod_richtext, weddingmodule WHERE
        mod_richtext.weddingid = {weddingid} AND
        weddingmodule.weddingid = mod_richtext.weddingid AND
        weddingmodule.moduleid = {moduleid}
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> RichText.ID
    ).as(richText.singleOpt)
  }

  /**
   * Update and save the rich text
   * @param richText the rich text with the new values
   */
  def edit(richText: RichText) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE mod_richtext SET
           content = {content}
          WHERE weddingid = {weddingid}
          """
        ).on(
          'content -> richText.moduleContent,
          'weddingid -> richText.weddingId
        ).executeUpdate()
    }
  }

  /**
   * Insert a rich text for a wedding
   * @param richText the rich text to insert
   */
  def add(richText: RichText)(implicit connection: Connection) = {
    SQL(
    """
        INSERT INTO mod_richtext (
          weddingid, content
        ) VALUES (
          {weddingid}, {content}
        )
    """
  ).on(
    'weddingid -> richText.weddingId,
    'content -> richText.moduleContent
  ).executeUpdate()
  }

  /**
   * Delete rich text for the given wedding
   * @param weddingId the wedding ID
   * @param richText the text
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], richText: RichText)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_richtext
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}