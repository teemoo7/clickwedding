package models.admin

import play.api.i18n.{Lang, Messages}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 14.03.13
 * Time: 10:48
 * To change this template use File | Settings | File Templates.
 */
case class Question(number: Int, question: String, answer: String)
case class Category(key: String, text: String)
case class QuestionList(category: Category, questions: Map[Int, Question])

object Question {
  val KEY_CATEGORY =  "main.admin.faq.%s.category"
  val KEY_QUESTION =  "main.admin.faq.%s.%s.q"
  val KEY_ANSWER =    "main.admin.faq.%s.%s.a"

  val GENERAL_KEY_CATEGORY = "general"
  val SECURITY_KEY_CATEGORY = "security"
  val GIFTLIST_KEY_CATEGORY = "giftlist"

  def getAllQuestionsByCategory(implicit lang: Lang): Map[String, QuestionList] = {
    val generalQuestions = getGeneralQuestions
    val securityQuestions = getSecurityQuestions
    val giftListQuestions = getGiftListQuestions
    Map(
      generalQuestions.category.key -> generalQuestions,
      securityQuestions.category.key -> securityQuestions,
      giftListQuestions.category.key -> giftListQuestions
    )
  }

  /**
   * Retrieve the general questions of the FAQ
   * @return a list of questions
   */
  def getGeneralQuestions(implicit lang: Lang): QuestionList = {
    getQuestions(GENERAL_KEY_CATEGORY)
  }

  /**
   * Retrieve the gift list / PayPal questions of the FAQ
   * @return a list of questions
   */
  def getGiftListQuestions(implicit lang: Lang): QuestionList = {
    getQuestions(GIFTLIST_KEY_CATEGORY)
  }

  /**
   * Retrieve the security questions of the FAQ
   * @return a list of questions
   */
  def getSecurityQuestions(implicit lang: Lang): QuestionList = {
    getQuestions(SECURITY_KEY_CATEGORY)
  }

  /**
   * Retrieve a map of questions given the category key, the questions key and the answers key
   */
  protected def getQuestions(categoryKey: String)(implicit lang: Lang): QuestionList = {
    val category = Messages(KEY_CATEGORY.format(categoryKey))
    var hasNext = true
    var i = 0
    var questions = collection.immutable.SortedMap[Int, Question]()
    while(hasNext) {
      val qKey = KEY_QUESTION.format(categoryKey, i)
      val q = Messages(qKey)
      if (q.equals(qKey)) {
        hasNext = false
      } else {
        val aKey = KEY_ANSWER.format(categoryKey, i)
        val a = Messages(aKey)
        questions += i -> Question(i, q, a)
      }
      i += 1
    }
    QuestionList(Category(categoryKey, category), questions)
  }
}