package controllers.admin

import controllers.AnyController
import play.api.mvc.Action
import play.api.i18n.Messages

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.03.13
 * Time: 16:50
 * To change this template use File | Settings | File Templates.
 */
object HelpCenter extends AnyController {
  /**
   * Display the terms and conditions page
   */
  def terms = Action {
    implicit request =>
      Ok(views.html.admin.help.terms())
  }

  /**
   * Display the provider-specific terms and conditions page
   */
  def termsProvider = Action {
    implicit request =>
      Ok(views.html.admin.help.termsProvider())
  }

  /**
   * Display the privacy policy page
   */
  def privacy = Action {
    implicit request =>
      Ok(views.html.admin.help.privacy())
  }

  /**
   * Display the FAQ page
   */
  def faq = Action {
    implicit request =>
      val allQuestions = models.admin.Question.getAllQuestionsByCategory
      Ok(views.html.admin.help.faq(allQuestions))
  }
}
