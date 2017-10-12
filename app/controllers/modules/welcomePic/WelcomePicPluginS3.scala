package controllers.modules.welcomePic

import play.api.Logger
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model._
import java.io.{File}
import scala.Some
import models.modules.{WelcomePicture}
import com.amazonaws.HttpMethod

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.03.13
 * Time: 16:57
 * To change this template use File | Settings | File Templates.
 */
class WelcomePicPluginS3(app : play.api.Application) extends WelcomePicPlugin {

  val AWS_S3_BUCKET = "aws.s3.welcomePic.bucket"
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

  def saveWelcomePicture(welcomePicture: WelcomePicture, file: File) {
    amazonS3 match {
      case Some(aws3) => {
        val metaData = new ObjectMetadata()
        welcomePicture.contentType match {
          case Some(c) => metaData.setContentType(c)
          case None =>
        }
        val request = new PutObjectRequest(bucket, welcomePicture.key, file).withCannedAcl(CannedAccessControlList.AuthenticatedRead).withMetadata(metaData)
        aws3.putObject(request)
      }
      case _ => {
        Logger.error("Could not save welcome picture to AWS3, amazonS3 is null")
        throw new RuntimeException("Could not save welcome picture");
      }
    }
  }

  def getWelcomePictureLink(welcomePicture: WelcomePicture): Option[String] = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new GeneratePresignedUrlRequest(bucket, welcomePicture.key, HttpMethod.GET)
        val headerOverrides = new ResponseHeaderOverrides()
        welcomePicture.contentType match {
          case Some(c) => headerOverrides.setContentType(c)
          case None =>
        }
        request.setResponseHeaders(headerOverrides)
        welcomePicture.filename match {
          case Some(f) => headerOverrides.setContentDisposition("filename=\""+f+"\"")
          case None =>
        }
        Some(aws3.generatePresignedUrl(request).toString)
      }
      case _ => {
        Logger.error("Could not get welcome picture from AWS3, amazonS3 is null")
        None
      }
    }
  }

  def deleteWelcomePicture(welcomePicture: WelcomePicture): Unit = {
    amazonS3 match {
      case Some(aws3) => {
        val request = new DeleteObjectRequest(bucket, welcomePicture.key)
        aws3.deleteObject(request)
      }
      case _ => {
        Logger.error("Could not get welcome picture from AWS3, amazonS3 is null")
        throw new RuntimeException("Could not delete picture");
      }
    }
  }
}