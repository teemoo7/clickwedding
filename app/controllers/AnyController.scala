package controllers

import authentication.Authorization
import captcha.TicTacToeCaptcha
import play.api.mvc.Controller
import play.api.data.Mapping
import play.api.data.Forms._
import play.api.data.validation._
import play.api.data.format.Formats._
import play.api.Play.current

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 23.01.13
 * Time: 16:51
 * To change this template use File | Settings | File Templates.
 */
trait AnyController extends Controller with Authorization with RestrictedArea with CookieLang with TicTacToeCaptcha with CookieViewMode with securesocial.core.SecureSocial {

  /**
   * This is almost the same as the 'email' mapping. However, the nonEmpty constraint is added, which simply allors the
   * FieldConstructor to add "*" next to the label of the field to inform that the field is required.
   */
  val nonEmptyEmail: Mapping[String] = of[String].verifying(Constraints.pattern(
    """\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r,
    "constraint.email",
    "error.email")).verifying(Constraints.nonEmpty)

  /**
   * Determine if the connection supports SSL or not
   * @return true if HTTPS
   */
  def isSecureConnection: Boolean = {
    play.Play.application().configuration().getBoolean("secure")
  }

  /**
   * Concatenate a list of HTML templates to a single HTML template
   * @param list a list of HTML templates
   * @return the concatenation of the templates
   */
  def mkHtml(list: List[play.api.templates.Html]): play.api.templates.Html = list match {
    case Nil => play.api.templates.Html("")
    case head :: Nil => head
    case list =>
      list.head += mkHtml(list.tail)
  }

  /**
   * Get a status information of the JDBC connections.
   *
   * @return The status information of the JDBC connections.
   */
  def getConnectionStatus = {
    val status = play.api.db.DB.getDataSource() match {
      case ds: com.jolbox.bonecp.BoneCPDataSource => {
        val bcp = ds.getPool()
        "JDBC connections: " + bcp.getTotalLeased + " in use / " + bcp.getTotalFree + " in pool / total created " + bcp.getTotalCreatedConnections
      }
      case _ => "unknown"
    }
    "[JDBC info] "+status
  }
}
