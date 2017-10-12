package controllers

import play.api.mvc.Controller
import play.api.Play.current
import play.api.cache.Cache

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 21.01.13
 * Time: 10:50
 * To change this template use File | Settings | File Templates.
 */

trait RestrictedArea extends Controller {

  /**
   * Cache key for restricted area code
   */
  private val RESTRICTED_AREA_CACHE_KEY = "restrictedArea"

  /**
   * Save the given code in the cache for an hour
   * @param code the code
   */
  def setCode(code: String) = {
    Cache.set(RESTRICTED_AREA_CACHE_KEY, code, 60*60)
  }

  /**
   * Return the code read from the cache if any
   * @return the code or None
   */
  def getCode(): Option[String] = {
    Cache.get(RESTRICTED_AREA_CACHE_KEY) match {
      case Some(code: String) => {
        // Renewal
        setCode(code)
        Some(code)
      }
      case _ => None
    }
  }

  /**
   * Delete the code from the cache
   */
  def removeCode(): Unit = {
    Cache.set(RESTRICTED_AREA_CACHE_KEY, null)
  }
}