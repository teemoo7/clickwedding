package controllers.admin

import controllers.AnyController
import play.api.mvc.Action

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.03.13
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
object Console extends AnyController {

  val NAV_PAGE_HOME = "NAV_PAGE_HOME"
  val NAV_PAGE_SITE_STATISTICS = "NAV_PAGE_SITE_STATISTICS"
  val NAV_PAGE_WEDDINGS_LIST = "NAV_PAGE_WEDDINGS_LIST"
  val NAV_PAGE_PROMO_CODES = "NAV_PAGE_PROMO_CODES"

  /**
   * Display the admin console
   */
  def display = SecuredAction(IsAdministrator()) {
    implicit request =>
      Ok(views.html.admin.console.display())
  }

  /**
   * Display the statistics page
   */
  def stats = SecuredAction(IsAdministrator()) {
    implicit request => {
      val stats = models.admin.stats.Stats.getStats
      Ok(views.html.admin.console.stats(stats))
    }
  }

}
