package controllers

import play.api.mvc.{Action, Controller, Cookie, RequestHeader}
import play.api.i18n.Lang
import securesocial.core.RequestWithUser

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 21.01.13
 * Time: 10:50
 * To change this template use File | Settings | File Templates.
 */

trait CookieLang extends Controller {

  /**
   * Change the locale cookie and refresh the current page with the new locale
   * @param locale the new locale
   */
  def changeLocale(locale: String) = Action { implicit request =>
    val referrer = request.headers.get(REFERER).getOrElse(HOME_URL)
    Redirect(referrer).withCookies(Cookie(LANG, locale))
  }

  /**
   * Return the locale (language) to be used (override the Controller method) given the cookie value if exists.
   */
  override implicit def lang(implicit request: RequestHeader) = {
    request.cookies.get(LANG) match {
      case None => super.lang(request)
      case Some(cookie) => Lang(cookie.value)
    }
  }

  implicit def headerFromRequestWithUser[A](implicit r: RequestWithUser[A]): RequestHeader = r.request

  /**
   * Cookie key
   */
  private val LANG = "lang"

  /**
   * Home url
   */
  private val HOME_URL = "/"
}