package controllers.helpers

import models.authentication.User
import play.api.i18n.Lang
import controllers.captcha.TicTacToeCaptcha

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.10.12
 * Time: 13:14
 * To change this template use File | Settings | File Templates.
 */
object CaptchaHelper extends TicTacToeCaptcha {

  def displayCaptcha(uuid: String, field: play.api.data.Field)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang) = {
    getCaptcha(uuid, field)
  }

  def getHeadInsert(uuid: String)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang) = {
    getCaptchaHead(uuid)
  }
}