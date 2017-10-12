package controllers.modules.customize

import java.io.File
import models.providers.{ProviderShowroomPicture, ProviderLogoPicture}
import models.modules.CustomizePicture

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 19.03.13
 * Time: 16:24
 * To change this template use File | Settings | File Templates.
 */
trait CustomizePicturePlugin extends play.api.Plugin {
  def savePicture(picture: CustomizePicture, file: File)
  def getPictureLink(picture: CustomizePicture): Option[String]
  def deletePicture(picture: CustomizePicture): Unit
}