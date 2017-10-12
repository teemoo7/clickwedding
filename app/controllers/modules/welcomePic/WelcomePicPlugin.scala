package controllers.modules.welcomePic

import models.modules.WelcomePicture
import java.io.{InputStream, File}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 19.03.13
 * Time: 16:24
 * To change this template use File | Settings | File Templates.
 */
trait WelcomePicPlugin extends play.api.Plugin {
  def saveWelcomePicture(picture: WelcomePicture, file: File)
  def getWelcomePictureLink(picture: WelcomePicture): Option[String]
  def deleteWelcomePicture(picture: WelcomePicture): Unit
}