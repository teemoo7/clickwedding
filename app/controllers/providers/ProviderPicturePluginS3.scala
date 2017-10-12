package controllers.providers

import play.api.Logger
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model._
import java.io.File
import scala.Some
import com.amazonaws.HttpMethod
import models.providers.{ProviderShowroomPicture, ProviderLogoPicture}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.03.13
 * Time: 16:57
 * To change this template use File | Settings | File Templates.
 */
class ProviderPicturePluginS3(app : play.api.Application) extends ProviderPicturePlugin {

  val AWS_S3_LOGO_BUCKET = "aws.s3.providers.logo.bucket"
  val AWS_S3_LOGO_SHOWROOM = "aws.s3.providers.showroom.bucket"
  val AWS_ACCESS_KEY = "aws.access.key"
  val AWS_SECRET_KEY = "aws.secret.key"

  var amazonS3: Option[AmazonS3Client] = None
  var logoBucket: String = null
  var showroomBucket: String = null

  override def onStart = {
    val accessKey = play.Play.application.configuration.getString(AWS_ACCESS_KEY)
    val secretKey = play.Play.application.configuration.getString(AWS_SECRET_KEY)
    val s3LogoBucket = play.Play.application.configuration.getString(AWS_S3_LOGO_BUCKET)
    val s3ShowroomBucket = play.Play.application.configuration.getString(AWS_S3_LOGO_SHOWROOM)

    if ((accessKey != null) && (secretKey != null)) {
      val awsCredentials = new com.amazonaws.auth.BasicAWSCredentials(accessKey, secretKey)
      amazonS3 = Some(new AmazonS3Client(awsCredentials))
      logoBucket = s3LogoBucket
      showroomBucket = s3ShowroomBucket
    }
  }

  override def enabled = {
     (play.Play.application.configuration.keys.contains(AWS_ACCESS_KEY) &&
       play.Play.application.configuration.keys.contains(AWS_SECRET_KEY) &&
       play.Play.application.configuration.keys.contains(AWS_S3_LOGO_BUCKET) &&
       play.Play.application.configuration.keys.contains(AWS_S3_LOGO_SHOWROOM))
  }

  def saveProviderLogoPicture(logoPicture: ProviderLogoPicture, file: File) {
    amazonS3 match {
      case Some(aws3) => {
        val metaData = new ObjectMetadata()
        logoPicture.contentType match {
          case Some(c) => metaData.setContentType(c)
          case None =>
        }
        val request = new PutObjectRequest(logoBucket, logoPicture.key, file).withCannedAcl(CannedAccessControlList.AuthenticatedRead).withMetadata(metaData)
        aws3.putObject(request)
      }
      case _ => {
        Logger.error("Could not save providers logo picture to AWS3, amazonS3 is null")
        throw new RuntimeException("Could not save providers logo picture");
      }
    }
  }

  def getProviderLogoPictureLink(logoPicture: ProviderLogoPicture): Option[String] = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GeneratePresignedUrlRequest(logoBucket, logoPicture.key, HttpMethod.GET)
        val headerOverrides = new ResponseHeaderOverrides()
        logoPicture.contentType match {
          case Some(c) => headerOverrides.setContentType(c)
          case None =>
        }
        request.setResponseHeaders(headerOverrides)
        logoPicture.filename match {
          case Some(f) => headerOverrides.setContentDisposition("filename=\""+f+"\"")
          case None =>
        }
        Some(aws3.generatePresignedUrl(request).toString)
      }
      case _ => {
        Logger.error("Could not get providers logo picture from AWS3, amazonS3 is null")
        None
      }
    }
  }

  def deleteProviderLogoPicture(logoPicture: ProviderLogoPicture): Unit = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new DeleteObjectRequest(logoBucket, logoPicture.key)
        aws3.deleteObject(request)
      }
      case _ => {
        Logger.error("Could not get providers logo picture from AWS3, amazonS3 is null")
        throw new RuntimeException("Could not delete providers logo picture");
      }
    }
  }

  def saveProviderShowroomPicture(showPicture: ProviderShowroomPicture, file: File, thumb: File) {
    amazonS3 match {
      case Some(aws3) => {
        val metaData = new ObjectMetadata()
        showPicture.contentType match {
          case Some(c) => metaData.setContentType(c)
          case None =>
        }
        val request = new PutObjectRequest(showroomBucket, showPicture.key, file).withCannedAcl(CannedAccessControlList.AuthenticatedRead).withMetadata(metaData)
        aws3.putObject(request)
        val requestThumb = new PutObjectRequest(showroomBucket, showPicture.thumbKey, thumb).withCannedAcl(CannedAccessControlList.AuthenticatedRead).withMetadata(metaData)
        aws3.putObject(requestThumb)
      }
      case _ => {
        Logger.error("Could not save providers picture to AWS3, amazonS3 is null")
        throw new RuntimeException("Could not save providers picture");
      }
    }
  }

  def getProviderShowroomPictureLink(showPicture: ProviderShowroomPicture): Option[String] = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GeneratePresignedUrlRequest(showroomBucket, showPicture.key, HttpMethod.GET)
        val headerOverrides = new ResponseHeaderOverrides()
        showPicture.contentType match {
          case Some(c) => headerOverrides.setContentType(c)
          case None =>
        }
        request.setResponseHeaders(headerOverrides)
        showPicture.filename match {
          case Some(f) => headerOverrides.setContentDisposition("filename=\""+f+"\"")
          case None =>
        }
        Some(aws3.generatePresignedUrl(request).toString)
      }
      case _ => {
        Logger.error("Could not get providers picture from AWS3, amazonS3 is null")
        None
      }
    }
  }

  def getProviderShowroomPictureThumbnailLink(showPicture: ProviderShowroomPicture): Option[String] = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GeneratePresignedUrlRequest(showroomBucket, showPicture.thumbKey, HttpMethod.GET)
        val headerOverrides = new ResponseHeaderOverrides()
        showPicture.contentType match {
          case Some(c) => headerOverrides.setContentType(c)
          case None =>
        }
        request.setResponseHeaders(headerOverrides)
        showPicture.filename match {
          case Some(f) => headerOverrides.setContentDisposition("filename=\"thumb_"+f+"\"")
          case None =>
        }
        Some(aws3.generatePresignedUrl(request).toString)
      }
      case _ => {
        Logger.error("Could not get providers picture thumbnail link from AWS3, amazonS3 is null")
        None
      }
    }
  }

  def deleteProviderShowroomPicture(showPicture: ProviderShowroomPicture): Unit = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new DeleteObjectRequest(showroomBucket, showPicture.key)
        aws3.deleteObject(request)
        val requestThumb = new DeleteObjectRequest(showroomBucket, showPicture.thumbKey)
        aws3.deleteObject(requestThumb)
      }
      case _ => {
        Logger.error("Could not get providers picture from AWS3, amazonS3 is null")
        throw new RuntimeException("Could not delete providers picture");
      }
    }
  }
}