package controllers.helpers

import controllers.Wedding
import java.util.Date

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.10.12
 * Time: 13:14
 * To change this template use File | Settings | File Templates.
 */
object UtilsHelper {

  /**
   * Determine if a given optional value is defined and not empty
   * @param value the optional value
   * @return true if defined and not empty
   */
  def isNotEmpty(value: Option[Any]) = {
    value.isDefined && !value.isEmpty && !value.get.toString.isEmpty
  }

  /**
   * Determine if the given string is a Double
   * @param s the string
   * @return true if the string is a Double
   */
  def isDouble(s: String) = {
    try {
      s.toDouble
      true
    } catch {
      case _: Throwable =>
        false
    }
  }

  /**
   * Determine if the connection is secured (HTTPS)
   * @return true if SSL is enabled
   */
  def isSecureConnection = Wedding.isSecureConnection

  /**
   * Return a URL without the http:// or https:// in front
   * @param url
   * @return
   */
  def removeMethodToUrl(url: String) = {
    val s = url.trim.split("://")
    val address =
      if (s.length > 1) {
        s(1)
      } else {
        url
      }
    if (address.length > 0 && address.last == '/') {
      address.substring(0, address.length-1)
    } else {
      address
    }
  }

  /**
   * Format a date to a dd.MM.yyyy string
   */
  def dateToString(date: Date): String = {
    new java.text.SimpleDateFormat("dd.MM.yyyy").format(date)
  }

  /**
   * Determine if the current version is still in test or production-ready
   * @return
   */
  def isBetaTest = play.Play.application().configuration().getBoolean("isBetaTest")

  /**
   * Normalize a text (replace é by e for example)
   * @param s the text to normalize
   */
  def normalize(s: String): String = {
    val n = java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD)
    val result = n.replaceAll("\\p{InCombiningDiacriticalMarks}+", "").replaceAll("[^\\w\\.]", "")
    result
  }
}