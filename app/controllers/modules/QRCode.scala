package controllers.modules

import models.wedding.Wedding
import controllers.AnyController
import play.api.i18n.Messages
import java.util.Calendar

import java.io.{ByteArrayOutputStream, File, IOException, UnsupportedEncodingException}
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CharacterCodingException
import java.nio.charset.Charset
import java.nio.charset.CharsetEncoder
import java.util.Hashtable

import com.google.zxing.EncodeHintType
import com.google.zxing.MultiFormatWriter
import com.google.zxing.client.j2se.MatrixToImageWriter
import com.google.zxing.common._
import play.api.libs.Files.TemporaryFile
import play.api.libs.ws.WS
import views.html.helper

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 08.07.13
 * Time: 09:44
 * To change this template use File | Settings | File Templates.
 */

object QRCode extends AnyController {
  val PNG_MIME_TYPE = "image/png"
  val CHARSET_UTF8 = "UTF-8"

  /**
   * Generate the QR Code for the wedding event (date, location) to be added to a calendar
   * @param uid
   */
  def generateCalendarEvent(uid: String) = UserAwareAction {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => Ok(generate(getCalendarEvent(wedding))).as(PNG_MIME_TYPE)
      }.getOrElse(NotFound)
  }

  /**
   * Generate the ICS file for the wedding event (date, location) to be added to a calendar
   * @param uid
   */
  def generateCalendarEventICS(uid: String) = UserAwareAction {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => Ok(getCalendarEvent(wedding)).as("text/Calendar").withHeaders(("Content-Disposition", "attachment; filename=\"wedding.ics\""))
      }.getOrElse(NotFound)
  }

  /**
   * Return the event raw text
   */
  def getCalendarEventRaw(wedding: Wedding)(implicit request: play.api.mvc.RequestHeader): String = {
    getCalendarEvent(wedding)
  }

  /**
   * Send the image file of the calendar event QR Code (download)
   * @param uid the wedding UID
   */
  def getCalendarQRCode(uid: String) =  SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          val url = "https://chart.googleapis.com/chart?cht=qr&chs=125x125&chl="+helper.urlEncode(controllers.modules.QRCode.getCalendarEventRaw(wedding))
          val source = scala.io.Source.fromURL(url)(scala.io.Codec.ISO8859)
          val byteArray = source.map(_.toByte).toArray
          source.close()
          Ok(byteArray).as("image/png").withHeaders(("Content-Disposition", "attachment; filename=\"qrcode.png\""))
      }.getOrElse(NotFound)
  }

  /**
   * Send the image file of the website URL QR Code (download)
   * @param uid the wedding UID
   */
  def getURLQRCode(uid: String) =  SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          val url = "https://chart.googleapis.com/chart?cht=qr&chs=125x125&chl="+helper.urlEncode(controllers.routes.Wedding.display(wedding.uid).absoluteURL(controllers.helpers.UtilsHelper.isSecureConnection))
          val source = scala.io.Source.fromURL(url)(scala.io.Codec.ISO8859)
          val byteArray = source.map(_.toByte).toArray
          source.close()
          Ok(byteArray).as("image/png").withHeaders(("Content-Disposition", "attachment; filename=\"qrcode.png\""))
      }.getOrElse(NotFound)
  }

  /**
   * Generate the text corresponding to the wedding date within a calendar-ready format
   * @param wedding the wedding
   */
  protected def getCalendarEvent(wedding: Wedding)(implicit request: play.api.mvc.RequestHeader): String = {
    val brideAndGroom = (wedding.person1.isDefined && wedding.person2.isDefined) match {
      case true => Some(wedding.person1.get + " & " + wedding.person2.get)
      case false => None
    }

    val summary = "SUMMARY:"+Messages("main.wedding.wedding")+ (brideAndGroom match {
      case Some(names) => " "+names
      case None => ""
    }) + "\n"

    val description = "DESCRIPTION:"+controllers.routes.Wedding.display(wedding.uid).absoluteURL(isSecureConnection)+"\n"

    val location = wedding.place match {
      case Some(place) => Some("LOCATION:"+place+"\n")
      case None => None
    }

    val dateFormat = new java.text.SimpleDateFormat("yyyyMMdd")
    val date = wedding.date match {
      case Some(date) => {
        val c = Calendar.getInstance()
        c.setTime(date)
        c.add(Calendar.DATE, 1)
        val datePlusOne = c.getTime
        Some("DTSTART;VALUE=DATE:"+dateFormat.format(date)+"\nDTEND;VALUE=DATE:"+dateFormat.format(datePlusOne)+"\n")
      }
      case None => None
    }

    "BEGIN:VEVENT\n"+
      summary+
      date.getOrElse("") +
      location.getOrElse("") +
      description +
      "END:VEVENT"
  }

  /**
   * Generate the text corresponding to the wedding URL
   * @param wedding the wedding
   */
  protected def getWeddingUrl(wedding: Wedding)(implicit request: play.api.mvc.RequestHeader): String = {
    controllers.routes.Wedding.display(wedding.uid).absoluteURL(isSecureConnection)
  }

  /**
   * Generate the QRCode byte array given the content text
   * @param text the text to be inserted in the QRCode
   * @return an array of bytes
   */
  protected def generate(text: String): Array[Byte] = {
    net.glxn.qrgen.QRCode.from(text).to(net.glxn.qrgen.image.ImageType.PNG).withCharset(CHARSET_UTF8).stream().toByteArray
//    generateUTF8(text)
  }

  /**
   * Generate the QRCode byte array given the content text with UTF-8 encoding
   * @param text the text to be inserted in the QRCode
   * @return an array of bytes
   */
  protected def generateUTF8(text: String): Array[Byte] = {
    val charset = Charset.forName(CHARSET_UTF8)
    val encoder = charset.newEncoder()
      // Convert a string to UTF-8 bytes in a ByteBuffer
    val bbuf = encoder.encode(CharBuffer.wrap(text))
    val b = bbuf.array()

    val data = new String(b, CHARSET_UTF8)
    // get a byte matrix for the data
    val h = 125
    val w = 125
    val writer = new com.google.zxing.MultiFormatWriter()
    var hints = new Hashtable[EncodeHintType, String](2)
    hints.put(EncodeHintType.CHARACTER_SET, CHARSET_UTF8)
    val matrix = writer.encode(data, com.google.zxing.BarcodeFormat.QR_CODE, w, h, hints)
    val array = new ByteArrayOutputStream()
    MatrixToImageWriter.writeToStream(matrix, "PNG", array)
    array.toByteArray
  }

}
