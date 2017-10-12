package controllers.modules.gallery

import models.modules.Picture
import java.io.{InputStream, File}
import java.awt.image.BufferedImage

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 19.03.13
 * Time: 16:24
 * To change this template use File | Settings | File Templates.
 */
trait GalleryPlugin extends play.api.Plugin {
  def savePicture(picture: Picture, file: File, thumb: File)
  def getPicture(picture: Picture): InputStream
  def getPictureLink(picture: Picture): Option[String]
  def getPictureThumbnailLink(picture: Picture): Option[String]
  def deletePicture(picture: Picture): Unit
}