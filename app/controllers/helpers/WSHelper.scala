package controllers.helpers

import collection.mutable
import play.api.Logger

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.10.12
 * Time: 13:14
 * To change this template use File | Settings | File Templates.
 */
object WSHelper {
  val ENCODING = "UTF-8"

  /**
   * Parse a text line parameters of the form key1=value1&key2=value2 to return a map of parameters
   * @param params the input params line
   * @return a parameter map
   */
  def parseParameters(params: String): mutable.HashMap[String, String] = {
    val result = mutable.HashMap[String, String]()
    params.split("&").toList.map(_.trim).filter(_.length > 0).map { nameVal =>
      nameVal.split("=").toList match {
        case Nil =>
        case n :: v :: _ => result.put(java.net.URLDecoder.decode(n, ENCODING), java.net.URLDecoder.decode(v, ENCODING))
        case n :: _ => result.put(java.net.URLDecoder.decode(n, ENCODING), "")
      }
    }
    result
  }

  /**
   * Format a map of parameters into a HTTP request parameters string
   * @param params the parameters
   * @return a string in the form key1=value1&key2=name2
   */
  def formatRequest(params: mutable.HashMap[String, String]): String = {
    params.view map {
      case (key, value) => java.net.URLEncoder.encode(key, ENCODING) + "=" + java.net.URLEncoder.encode(value, ENCODING)
    } mkString ("&")
  }

  /**
   * Read and log the errors returned by PayPal
   * @param params the response parameters
   */
  def getAndLogPayPalError(params: mutable.HashMap[String, String]) = {
    val n = 0
    val errorCode = params.get("L_ERRORCODE"+n.toString)
    val severityCode = params.get("L_SEVERITYCODE"+n.toString)
    val shortMessage = params.get("L_SHORTMESSAGE"+n.toString)
    val longMessage = params.get("L_LONGMESSAGE"+n.toString)
    Logger.error("[PayPal Error] \t"+errorCode.getOrElse("")+" : "+severityCode.getOrElse("")+" : "+shortMessage.getOrElse(""));
    Logger.error("[PayPal Error] \t"+longMessage.getOrElse(""));
    Logger.error("[PayPal Error] \t"+params.toSeq.toString);
  }

  /**
   * Read and log the error envelope returned by PayPal
   * @param params the response parameters
   */
  def getAndLogPayPalErrorEnvelope(params: mutable.HashMap[String, String]) = {
    val n = 0
    val errorCode = params.get("error("+n.toString+").errorId")
    val severityCode = params.get("error("+n.toString+").severity")
    val longMessage = params.get("error("+n.toString+").parameter(0)")
    val shortMessage = params.get("error("+n.toString+").message")
    Logger.error("[PayPal Error] \t"+errorCode.getOrElse("")+" : "+severityCode.getOrElse("")+" : "+shortMessage.getOrElse(""));
    Logger.error("[PayPal Error] \t"+longMessage.getOrElse(""));
    Logger.error("[PayPal Error] \t"+params.toSeq.toString);
  }
}