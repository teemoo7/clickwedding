package utils

import securesocial.core.providers.utils.PasswordValidator
import play.api.i18n.{Lang, Messages}
import play.Application
import play.api.mvc.RequestHeader
import controllers.authentication.Authentication
import controllers.AnyController

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 05.03.13
 * Time: 14:39
 * To change this template use File | Settings | File Templates.
 */
class BetterPasswordValidator (application: Application) extends PasswordValidator {
  import BetterPasswordValidator._

  def isValid(password: String): Boolean = {
    password.matches(pattern)
  }
  def errorMessage = {
    Messages("securesocial.signup.invalidPassword", MIN_LENGTH)
  }

}

object BetterPasswordValidator {
  val MIN_LENGTH = 8
  // At least 1 numeric, 1 alpha (capital or not), 1 special char
//  val pattern = "^(?=.*[0-9])(?=.*[a-zA-Z])(?=.*[\\-_@#\\+\\$!\\?\\.\\*\\(\\)\\[\\]\\^])\\S{"+MIN_LENGTH+",}$"
  // At least 1 numeric, 1 alpha (capital or not)
  val pattern = "^(?=.*[0-9])(?=.*[a-zA-Z])\\S{"+MIN_LENGTH+",}$"
}