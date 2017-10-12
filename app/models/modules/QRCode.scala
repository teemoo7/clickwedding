package models.modules

import anorm._
import play.api.db.DB
import java.sql.Connection
import play.api.i18n.{Lang, Messages}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 21.01.13
 * Time: 09:42
 * To change this template use File | Settings | File Templates.
 */
case class QRCode(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long])
  extends Module(moduleId, moduleName, null, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SMALL_AND_LARGE, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_LEFT)), displayPreviousModuleId) {
  override def init()(implicit connection: Connection) = {}
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {}
  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = Some(Messages("main.modules.qrCode.addItToYourCalendar"))
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    views.html.modules.qrCode.display(wedding)
  }
}

object QRCode {
  val ID = 11
  val NAME = "QRCode"
}
