package controllers.authentication

import play.Application
import securesocial.controllers.TemplatesPlugin
import play.api.mvc.{RequestHeader, Request}
import play.api.data.Form
import play.api.templates.{Txt, Html}
import securesocial.controllers.Registration.RegistrationInfo
import securesocial.core.{Identity, SecuredRequest}
import securesocial.controllers.PasswordChange.ChangeInfo

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 05.03.13
 * Time: 17:02
 * To change this template use File | Settings | File Templates.
 */
class AuthenticationViewsPlugin(application: Application) extends TemplatesPlugin
{
  override def getLoginPage[A](implicit request: Request[A], form: Form[(String, String)],
                               msg: Option[String] = None): Html =
  {
    views.html.authentication.fullLogin(form, msg)(request = request, lang = controllers.Wedding.lang)
  }

  override def getSignUpPage[A](implicit request: Request[A], form: Form[RegistrationInfo], token: String): Html = {
    views.html.authentication.signUp(form, token)(request = request, lang = controllers.Wedding.lang)
  }

  override def getStartSignUpPage[A](implicit request: Request[A], form: Form[String]): Html = {
    views.html.authentication.startSignUp(form)(request = request, lang = controllers.Wedding.lang)
  }

  override def getStartResetPasswordPage[A](implicit request: Request[A], form: Form[String]): Html = {
    views.html.authentication.startResetPassword(form)(request = request, lang = controllers.Wedding.lang)
  }

  def getResetPasswordPage[A](implicit request: Request[A], form: Form[(String, String)], token: String): Html = {
    views.html.authentication.resetPassword(form, token)(request = request, lang = controllers.Wedding.lang)
  }

  def getPasswordChangePage[A](implicit request: SecuredRequest[A], form: Form[ChangeInfo]):Html = {
    views.html.authentication.passwordChange(form)(request = request, lang = controllers.Wedding.lang)
  }

  def getNotAuthorizedPage[A](implicit request: Request[A]): Html = {
    views.html.authentication.notAuthorized()(request = request, lang = controllers.Wedding.lang)
  }

  def getSignUpEmail(token: String)(implicit request: RequestHeader): (Option[Txt], Option[Html]) = {
    (None, Some(views.html.authentication.mails.signUpEmail(token)(request = request, lang = controllers.Wedding.lang)))
  }

  def getAlreadyRegisteredEmail(user: Identity)(implicit request: RequestHeader): (Option[Txt], Option[Html]) = {
    (None, Some(views.html.authentication.mails.alreadyRegisteredEmail(user)(request = request, lang = controllers.Wedding.lang)))
  }

  def getWelcomeEmail(user: Identity)(implicit request: RequestHeader): (Option[Txt], Option[Html]) = {
    (None, Some(views.html.authentication.mails.welcomeEmail(user)(request = request, lang = controllers.Wedding.lang)))
  }

  def getUnknownEmailNotice()(implicit request: RequestHeader): (Option[Txt], Option[Html]) = {
    (None, Some(views.html.authentication.mails.unknownEmailNotice()(request = request, lang = controllers.Wedding.lang)))
  }

  def getSendPasswordResetEmail(user: Identity, token: String)(implicit request: RequestHeader): (Option[Txt], Option[Html]) = {
    (None, Some(views.html.authentication.mails.passwordResetEmail(user, token)(request = request, lang = controllers.Wedding.lang)))
  }

  def getPasswordChangedNoticeEmail(user: Identity)(implicit request: RequestHeader): (Option[Txt], Option[Html]) = {
    (None, Some(views.html.authentication.mails.passwordChangedNotice(user)(request = request, lang = controllers.Wedding.lang)))
  }

}
