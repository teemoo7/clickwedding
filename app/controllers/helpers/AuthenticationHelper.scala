package controllers.helpers

import models.authentication.User
import controllers.CookieViewMode
import play.api.i18n.Lang

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.10.12
 * Time: 13:14
 * To change this template use File | Settings | File Templates.
 */
object AuthenticationHelper extends CookieViewMode {

  def isAuthenticated(implicit user: Option[User], lang: Lang): Boolean = {
    user.isDefined
  }

  def isOwnerOfWedding(uid: String)(implicit user: Option[User], request: play.api.mvc.RequestHeader, lang: Lang): Boolean = {
    user.map {
      _ => controllers.Wedding.isOwnerOfWedding(uid)(user.get) || isUserAdmin
    }.getOrElse(false) match {
      case true =>
        !isViewModeGuest
      case _ => false
    }
  }

  def isDisplayAsOwner(implicit request: play.api.mvc.RequestHeader, lang: Lang, isOwnerOfWedding: Boolean, viewMode: models.wedding.ViewMode): Boolean = {
    viewMode match {
      case models.wedding.ViewMode.OwnerViewMode => {
        isOwnerOfWedding
      }
      case _ => false
    }
  }

  def isAdmin(implicit user: User, lang: Lang): Boolean = {
    controllers.Wedding.isAdministrator()(user)
  }

  def isUserAdmin(implicit user: Option[User], lang: Lang): Boolean = {
    user.map {
      _ => controllers.Wedding.isAdministrator()(user.get)
    }.getOrElse(false)
  }

  def isGuestWithRestrictedArea(uid: String)(implicit user: Option[User], lang: Lang): Boolean = {
    user match {
      case Some(_) => controllers.Wedding.isGuestWithRestrictedArea(uid)(user.get) || isUserAdmin
      case None => controllers.Wedding.isGuestWithRestrictedArea(uid)(null)
    }
  }

  def isRestrictedAreaDefined(wedding: models.wedding.Wedding)(implicit lang: Lang): Boolean  = {
    wedding.code.isDefined
  }

  def isOwnerOfProvider(id: Long)(implicit user: Option[User], request: play.api.mvc.RequestHeader, lang: Lang): Boolean = {
    user.map {
      identity => controllers.Wedding.IsOwnerOfServiceProvider(id).isAuthorized(identity)
    }.getOrElse(false)
  }
}