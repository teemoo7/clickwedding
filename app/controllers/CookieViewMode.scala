package controllers

import play.api.mvc._
import play.api.i18n.Lang
import play.api.mvc.Cookie
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 21.01.13
 * Time: 10:50
 * To change this template use File | Settings | File Templates.
 */

trait CookieViewMode extends Controller {
  /**
   * Cookie key
   */
  private val KEY_VIEW_MODE = "viewMode"

  /**
   * Cookie view values
   */
  private val VIEW_MODE_GUEST = "guest"

  /**
   * Home url
   */
  private val HOME_URL = "/"

  /**
   * Display the wedding as if the user was a guest
   */
  def viewAsGuest = Action { implicit request =>
    val referrer = request.headers.get(REFERER).getOrElse(HOME_URL)
    Redirect(referrer).withCookies(Cookie(KEY_VIEW_MODE, VIEW_MODE_GUEST))
  }

  /**
   * Display the wedding normally (as the owner)
   */
  def viewAsOwner = Action { implicit request =>
    val referrer = request.headers.get(REFERER).getOrElse(HOME_URL)
    Redirect(referrer).discardingCookies(DiscardingCookie(KEY_VIEW_MODE))
  }

  /**
   * Implicitly set the current view mode
   */
  implicit def viewMode(implicit request: RequestHeader): models.wedding.ViewMode = {
    isViewModeGuest match {
      case true => models.wedding.ViewMode.GuestViewMode
      case false => models.wedding.ViewMode.OwnerViewMode
    }
  }

  /**
   * Determine if the wedding should be displayed as a guest's view or not
   */
  def isViewModeGuest(implicit request: RequestHeader) = {
    request.cookies.get(KEY_VIEW_MODE) match {
      case None => false
      case Some(mode) => mode.value.equals(VIEW_MODE_GUEST)
    }
  }
}
