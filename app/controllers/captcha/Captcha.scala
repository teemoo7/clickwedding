package controllers.captcha

import play.api.mvc.Controller
import models.authentication.User
import play.api.i18n.Lang
import play.api.templates.Html

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.12.12
 * Time: 08:29
 * To change this template use File | Settings | File Templates.
 */
abstract trait Captcha extends Controller {

  def generateCaptcha: String

  def validateCaptcha(value: String, uuid: String): Boolean

  def getCaptcha(uuid: String, field: play.api.data.Field)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang): Html

  def getCaptchaHead(uuid: String)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang): Html

}