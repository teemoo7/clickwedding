package controllers.modules.gallery

import play.api.{Logger, Plugin}
import com.amazonaws.services.s3.{S3ClientOptions, AmazonS3Client}
import com.amazonaws.services.s3.model._
import models.modules.Picture
import java.io.{InputStream, File}
import scala.Some
import models.modules.Picture
import com.amazonaws.HttpMethod
import com.sun.xml.internal.messaging.saaj.packaging.mime.internet.ContentDisposition
import java.awt.image.BufferedImage
import sun.misc.IOUtils

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.03.13
 * Time: 16:57
 * To change this template use File | Settings | File Templates.
 */
class GalleryPluginS3(app : play.api.Application) extends GalleryPlugin {

  val AWS_S3_BUCKET = "aws.s3.gallery.bucket"
  val AWS_ACCESS_KEY = "aws.access.key"
  val AWS_SECRET_KEY = "aws.secret.key"

  var amazonS3: Option[AmazonS3Client] = None
  var bucket: String = null

  override def onStart = {
    val accessKey = play.Play.application.configuration.getString(AWS_ACCESS_KEY)
    val secretKey = play.Play.application.configuration.getString(AWS_SECRET_KEY)
    val s3Bucket = play.Play.application.configuration.getString(AWS_S3_BUCKET)

    if ((accessKey != null) && (secretKey != null)) {
      val awsCredentials = new com.amazonaws.auth.BasicAWSCredentials(accessKey, secretKey)
      amazonS3 = Some(new AmazonS3Client(awsCredentials))
      bucket = s3Bucket
    }
  }

  override def enabled = {
     (play.Play.application.configuration.keys.contains(AWS_ACCESS_KEY) &&
       play.Play.application.configuration.keys.contains(AWS_SECRET_KEY) &&
       play.Play.application.configuration.keys.contains(AWS_S3_BUCKET))
  }

  def savePicture(picture: Picture, file: File, thumb: File) {
    amazonS3 match {
      case Some(aws3) => {
        val metaData = new ObjectMetadata()
        metaData.setContentType(picture.contentType)
        val request = new PutObjectRequest(bucket, picture.key, file).withCannedAcl(CannedAccessControlList.AuthenticatedRead).withMetadata(metaData)
        aws3.putObject(request)
        val thumbnailRequest = new PutObjectRequest(bucket, picture.thumbKey, thumb).withCannedAcl(CannedAccessControlList.AuthenticatedRead).withMetadata(metaData)
        aws3.putObject(thumbnailRequest)
      }
      case _ => {
        Logger.error("Could not save picture to AWS3, amazonS3 is null")
        throw new RuntimeException("Could not save picture");
      }
    }
  }

  def getPicture(picture: Picture): InputStream = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GetObjectRequest(bucket, picture.key)
        val file: File = null
        val obj = aws3.getObject(request)
        obj.getObjectContent
      }
      case _ => {
        Logger.error("Could not get picture from AWS3, amazonS3 is null")
        //None
        null
      }
    }
  }

  def getPictureLink(picture: Picture): Option[String] = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GeneratePresignedUrlRequest(bucket, picture.key, HttpMethod.GET)
        val headerOverrides = new ResponseHeaderOverrides()
        headerOverrides.setContentType(picture.contentType)
        request.setResponseHeaders(headerOverrides)
        headerOverrides.setContentDisposition("filename=\""+picture.filename+"\"")
        Some(aws3.generatePresignedUrl(request).toString)
      }
      case _ => {
        Logger.error("Could not get picture link from AWS3, amazonS3 is null")
        None
      }
    }
  }

  def getPictureThumbnailLink(picture: Picture): Option[String] = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GeneratePresignedUrlRequest(bucket, picture.thumbKey, HttpMethod.GET)
        val headerOverrides = new ResponseHeaderOverrides()
        headerOverrides.setContentType(picture.contentType)
        request.setResponseHeaders(headerOverrides)
        headerOverrides.setContentDisposition("filename=\"thumb_"+picture.filename+"\"")
        Some(aws3.generatePresignedUrl(request).toString)
      }
      case _ => {
        Logger.error("Could not get picture link from AWS3, amazonS3 is null")
        None
      }
    }
  }

  def deletePicture(picture: Picture): Unit = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new DeleteObjectRequest(bucket, picture.key)
        aws3.deleteObject(request)
        val requestThumb = new DeleteObjectRequest(bucket, picture.thumbKey)
        aws3.deleteObject(requestThumb)
      }
      case _ => {
        Logger.error("Could not get picture link from AWS3, amazonS3 is null")
        throw new RuntimeException("Could not delete picture");
      }
    }
  }
}