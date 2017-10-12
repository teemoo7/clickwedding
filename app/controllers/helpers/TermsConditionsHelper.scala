package controllers.helpers

import play.api.i18n.Lang
import java.io.{IOException, InputStream}
import play.api.Play
import play.api.Play.current

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.10.12
 * Time: 13:14
 * To change this template use File | Settings | File Templates.
 */
object TermsConditionsHelper {
  val LANGUAGE_FR = "fr"
  val TERMS_FILE_FR = "terms-fr.txt"
  val TERMS_FILE_EN = "terms-en.txt"
  val TERMS_PROVIDER_FILE_FR = "terms-provider-fr.txt"
  val TERMS_PROVIDER_FILE_EN = "terms-provider-en.txt"
  val PRIVACY_FILE_FR = "privacy-fr.txt"
  val PRIVACY_FILE_EN = "privacy-en.txt"

  /**
   * Get the terms and conditions given the language
   * @param lang the language
   * @return the terms and conditions as raw text
   */
  def getTermsAndConditions(lang: Lang)(implicit request: play.api.mvc.RequestHeader): String = {
    lang.language match {
      case LANGUAGE_FR => {
        load(TERMS_FILE_FR)
      }
      case _ => {
        load(TERMS_FILE_EN)
      }
    }
  }

  /**
   * Get the provider-specificc terms and conditions given the language
   * @param lang the language
   * @return the terms and conditions as raw text
   */
  def getTermsAndConditionsProvider(lang: Lang)(implicit request: play.api.mvc.RequestHeader): String = {
    lang.language match {
      case LANGUAGE_FR => {
        load(TERMS_PROVIDER_FILE_FR)
      }
      case _ => {
        load(TERMS_PROVIDER_FILE_EN)
      }
    }
  }

  /**
   * Get the privacy policy given the language
   * @param lang the language
   * @return the privacy polcy as raw text
   */
  def getPrivacyPolicy(lang: Lang)(implicit request: play.api.mvc.RequestHeader): String = {
    lang.language match {
      case LANGUAGE_FR => {
        load(PRIVACY_FILE_FR)
      }
      case _ => {
        load(PRIVACY_FILE_EN)
      }
    }
  }

  /**
   * Load a file and return its content
   * @param filePath the file to load
   * @return the file content
   */
  protected def load(filePath: String): String = {
    Play.resourceAsStream(filePath) match {
      case Some(is) => scala.io.Source.fromInputStream(is, "UTF-8").getLines().mkString("\n")
      case _ => throw new IOException("File not found: " + filePath)
    }
  }
}