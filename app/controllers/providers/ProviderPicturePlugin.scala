package controllers.providers

import models.modules.WelcomePicture
import java.io.File
import models.providers.{ProviderShowroomPicture, ProviderLogoPicture}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 19.03.13
 * Time: 16:24
 * To change this template use File | Settings | File Templates.
 */
trait ProviderPicturePlugin extends play.api.Plugin {
  def saveProviderLogoPicture(logoPicture: ProviderLogoPicture, file: File)
  def getProviderLogoPictureLink(logoPicture: ProviderLogoPicture): Option[String]
  def deleteProviderLogoPicture(logoPicture: ProviderLogoPicture): Unit
  def saveProviderShowroomPicture(showPicture: ProviderShowroomPicture, file: File, thumb: File)
  def getProviderShowroomPictureLink(showPicture: ProviderShowroomPicture): Option[String]
  def getProviderShowroomPictureThumbnailLink(showPicture: ProviderShowroomPicture): Option[String]
  def deleteProviderShowroomPicture(showPicture: ProviderShowroomPicture): Unit
}