package app.controllers.helpers

import java.awt.{RenderingHints, Image, Graphics2D, AlphaComposite}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io._
import play.api.Play
import models.modules.Picture
import models.modules.Picture

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 21.03.13
 * Time: 10:54
 * To change this template use File | Settings | File Templates.
 */
object ImagesHelper {
  /**
   * Resize an image
   * @param filename the file name (used for extension)
   * @param image the original image file
   * @param resizedImage the destination image file
   * @param width the image width
   * @param height the image height
   */
  def resize(filename: String, image: File, resizedImage: File, width: Int, height: Int) = {
    val bufferedImage = resizeWithJava(new FileInputStream(image), width, height)
    val extension = filename.substring(filename.lastIndexOf('.')+1)
    val formatType = extension.length > 0 match {
      case true => extension
      case false => "jpg"
    }
    ImageIO.write(bufferedImage, formatType, resizedImage)
  }

  /**
   * Resize an image (as an input stream) to a maximal size
   * @param is the image file input stream
   * @param maxWidth the max width
   * @param maxHeight the max height
   * @return the resized image as a buffered image
   */
  protected def resizeWithJava(is: java.io.InputStream, maxWidth: Int, maxHeight: Int): BufferedImage = {
    require (maxWidth > 0)
    require (maxHeight > 0)
    val originalImage:BufferedImage = ImageIO.read(is)

    var height = originalImage.getHeight
    var width = originalImage.getWidth

    // Shortcut to save a pointless reprocessing in case the image is small enough already
    if (width <= maxWidth && height <= maxHeight)
      originalImage
    else {
      // If the picture was too big, it will either fit by width or height.
      // This essentially resizes the dimensions twice, until it fits
      if (width > maxWidth){
        height = (height.doubleValue() * (maxWidth.doubleValue() / width.doubleValue())).intValue
        width = maxWidth
      }
      if (height > maxHeight){
        width = (width.doubleValue() * (maxHeight.doubleValue() / height.doubleValue())).intValue
        height = maxHeight
      }
      val scaledBI = new BufferedImage(width, height,  BufferedImage.TYPE_INT_RGB)
      val g = scaledBI.createGraphics
      g.setComposite(AlphaComposite.Src)
      g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.drawImage(originalImage, 0, 0, width, height, null);
      g.dispose
      scaledBI
    }
  }
}