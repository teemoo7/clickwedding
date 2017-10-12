import controllers.{AnyController, CookieLang}
import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.mvc.Results._
import scala.concurrent.Future

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 27.10.13
 * Time: 16:31
 * To change this template use File | Settings | File Templates.
 */

object Global extends WithFilters(EnforceHTTPSFilter) with CookieLang with AnyController {
  override def onError(request: RequestHeader, ex: Throwable) = {
    println(getConnectionStatus)
    InternalServerError(
      views.html.errors.globalError(ex)(play.api.mvc.Flash.emptyCookie, request, None, lang(request))
    )
  }
  override def onStart(app: Application) {
    play.api.db.DB.getDataSource() match {
      case ds: com.jolbox.bonecp.BoneCPDataSource => {
        ds.setDisableConnectionTracking(true)
      }
      case _ =>
    }
  }
}

/**
 * Filter request to force HTTPS (SSL). If the protocol is not https, then redirect to https
 */
object EnforceHTTPSFilter extends Filter {
  override def apply(next: RequestHeader => Result)(request: RequestHeader): Result = {
    if (Play.isProd && controllers.Wedding.isSecureConnection && !request.headers.get("x-forwarded-proto").getOrElse("").contains("https")) {
      play.api.mvc.Results.MovedPermanently("https://" + request.host + request.uri)
    } else {
      next(request)
    }
  }
}