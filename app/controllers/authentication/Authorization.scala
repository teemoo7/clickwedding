package controllers.authentication

import play.api.mvc.{Request, Controller}
import controllers.RestrictedArea
import securesocial.core.{AuthenticationMethod, SecuredRequest, Identity}
import anorm.Id

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 12.10.12
 * Time: 08:22
 * To change this template use File | Settings | File Templates.
 */
trait Authorization extends Controller with RestrictedArea {

  /**
   * User / password authorization
   */
  case class IsMethodUserPassword() extends securesocial.core.Authorization {
    /**
     * Check if the user is logged with the username / password method
     * @return true if it is the case
     */
    def isAuthorized(identity: Identity) = {
      identity.authMethod.is(AuthenticationMethod.UserPassword)
    }
  }

  /**
   * Wedding owner authorization
   * @param uid the wedding UID
   */
  case class IsOwnerOfWedding(uid: String) extends securesocial.core.Authorization {
    /**
     * Check if the user is the owner of the wedding, given its UID
     * @return true if the user is the owner
     */
    def isAuthorized(identity: Identity) = {
      identity match {
        case user: models.authentication.User =>
//          models.wedding.Wedding.findByUid(uid).map {
//            wedding => {
              models.wedding.Wedding.isOwner(uid, user.userId) || user.admin
//            }
//          }.getOrElse(false)
        case _ => false
      }
    }
  }

  /**
   * Site administrator authorization
   */
  case class IsAdministrator() extends securesocial.core.Authorization {
    /**
     * Check if the user is an administrator
     * @param identity the user
     * @return true if the user is admin
     */
    def isAuthorized(identity: Identity) = {
      identity match {
        case user: models.authentication.User => user.admin
        case _ => false
      }
    }
  }

  case class IsOwnerOfWeddingAndModuleFree(uid: String, moduleId: Long) extends securesocial.core.Authorization {
    /**
     * Check if the user if the owner of the wedding, given its UID, and if the module is free
     * @param identity the user
     * @return true if the user is the owner and module is free
     */
    def isAuthorized(identity: Identity): Boolean = {
      identity match {
        case user: models.authentication.User =>
          models.wedding.Wedding.findByUid(uid).map {
            wedding => {
              if (models.wedding.Wedding.isOwner(uid, user.userId)) {
                getModulePrice(moduleId) match {
                  case Some(price) => price == 0
                  case None => false
                }
              } else {
                false
              } || user.admin
            }
          }.getOrElse(false)
        case _ => false
      }
    }
  }


  /**
   * Service provider owner authorization
   * @param id the provider ID
   */
  case class IsOwnerOfServiceProvider(id: Long) extends securesocial.core.Authorization {
    /**
     * Check if the user is the owner of the service provider, given its ID
     * @return true if the user is the owner
     */
    def isAuthorized(identity: Identity) = {
      identity match {
        case user: models.authentication.User =>
          user.admin || models.providers.Provider.getProvidersByUserId(user).filter(_.isValid).find(_.id.get == id).isDefined
      }
    }
  }

  /**
   * Invoice service provider owner authorization
   * @param purchaseId the purchase ID
   */
  case class IsOwnerOfServiceProviderPurchase(purchaseId: Long) extends securesocial.core.Authorization {
    /**
     * Check if the user is the owner of the service provider for which the purchase was done
     * @return true if the user is the owner
     */
    def isAuthorized(identity: Identity) = {
      identity match {
        case user: models.authentication.User =>
          user.admin || (
            models.providers.Provider.getPurchase(Id(purchaseId)) match {
              case Some(purchase) => {
                models.providers.Provider.getProvidersByUserId(user).filter(_.isValid).find(_.id.get == purchase.providerId).isDefined
              }
              case None => false
            }
          )
      }
    }
  }

  /**
   * Automatically pass the optional user to the view if logged in
   */
  implicit def user[A](implicit request : Request[A]): Option[models.authentication.User] = {
    //todo: should not be a request but a RequestWithUser
    securesocial.core.SecureSocial.currentUser match {
      case Some(identity) => Some(identity.asInstanceOf[models.authentication.User])
      case _ => None
    }
  }

  /**
   * Check if the user if the owner of the wedding, given its UID
   * @param uid the wedding UID
   * @param user the user
   * @return true if the user is the owner
   */
  def isOwnerOfWedding(uid: String)(user: models.authentication.User): Boolean = {
//    models.wedding.Wedding.findByUid(uid).map {
//      wedding => {
        models.wedding.Wedding.isOwner(uid, user.userId) || user.admin
//      }
//    }.getOrElse(false)
  }

  /**
   * Check if the user is an administrator
   * @param user
   * @return true if the user is admin
   */
  def isAdministrator()(user: models.authentication.User) = {
    user.admin
  }

  /**
   * Check if the user is a normal registered user
   * @param user
   * @return true if the user is a normal user
   */
  def isNormalUser()(user: models.authentication.User) = {
    user != null
  }

  /**
   * Check if the user if the owner of the wedding, given its UID, and if the module is free
   * @param uid
   * @param user
   * @return true if the user is the owner and module is free
   */
  def isOwnerOfWeddingAndModuleFree(uid: String, moduleId: Long)(user: models.authentication.User): Boolean = {
//    models.wedding.Wedding.findByUid(uid).map {
//      wedding => {
        if (models.wedding.Wedding.isOwner(uid, user.userId) || user.admin) {
          getModulePrice(moduleId) match {
            case Some(price) => price == 0
            case None => false
          }
        } else {
          false
        }
//      }
//    }.getOrElse(false)
  }

  /**
   * Check if the user had entered a code to access the restricted access and if the code is correct for the given wedding.
   * Administrator and owners of the wedding always have access to this area.
   * @param uid the wedding UID
   * @param user the user
   * @return true if the user has access to the restricted area
   */
  def isGuestWithRestrictedArea(uid: String)(user: models.authentication.User): Boolean = {
//    models.wedding.Wedding.findByUid(uid).map {
//      wedding =>
        val code: Option[String] = models.wedding.Wedding.getWeddingCodeByUid(uid)
        (user != null && (models.wedding.Wedding.isOwner(uid, user.userId) || user.admin)) || code.map {
          weddingCode =>
            getCode().map{
              code =>
                weddingCode.equalsIgnoreCase(code)
            }.getOrElse(false)
        }.getOrElse(true)
//    }.getOrElse(false)
  }

  /**
   * Get the price of a given module if it exists
   * @param moduleId the module ID
   * @return Some int if found or None if module does not exist (or is disabled)
   */
  def getModulePrice(moduleId: Long): Option[Int] = {
    models.modules.Module.getModule(moduleId).map {
      module => Some(module.price)
    }.getOrElse(None)
  }
}
