package service

import play.api.{Logger, Application}
import securesocial.core.{AuthenticationMethod, UserServicePlugin, UserId, SocialUser, Identity}
import securesocial.core.providers.Token
import models.authentication.User

class UserService(application: Application) extends UserServicePlugin(application) {
  private var tokens = Map[String, Token]()

  /**
   * Finds a SocialUser that matches the specified id
   *
   * @param id the user id
   * @return an optional user
   */
  def find(id: UserId): Option[Identity] = {
    models.authentication.User.findBySocialUserId(id)
  }

  /**
   * Finds a Social user by email and provider id.
   *
   * Note: If you do not plan to use the UsernamePassword provider just provide en empty
   * implementation.
   *
   * @param email - the user email
   * @param providerId - the provider id
   * @return
   */
  def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    find(UserId(email, providerId))
  }

  /**
   * Saves the user.  This method gets called when a user logs in.
   * This is your chance to save the user information in your backing store.
   * @param identity
   */
  def save(identity: Identity): Identity = {
    find(identity.id) match {
      case Some(u: User) => {
        models.authentication.User.updateUser(User(identity).copy(userId = u.userId, lastLogin = u.lastLogin, admin = u.admin))
      }
      case _ => {
        models.authentication.User.createUser(identity).getOrElse(identity)
      }
    }
  }

  /**
   * Saves a token.  This is needed for users that
   * are creating an account in the system instead of using one in a 3rd party system.
   *
   * Note: If you do not plan to use the UsernamePassword provider just provide en empty
   * implementation
   *
   * @param token The token to save
   * @return A string with a uuid that will be embedded in the welcome email.
   */
  def save(token: Token) {
    tokens += (token.uuid -> token)
  }

  /**
   * Finds a token
   *
   * Note: If you do not plan to use the UsernamePassword provider just provide en empty
   * implementation
   *
   * @param token the token id
   * @return
   */
  def findToken(token: String): Option[Token] = {
    tokens.get(token)
  }

  /**
   * Deletes a token
   *
   * Note: If you do not plan to use the UsernamePassword provider just provide en empty
   * implementation
   *
   * @param uuid the token id
   */
  def deleteToken(uuid: String) {
    tokens -= uuid
  }

  def deleteTokens() {
    tokens = Map()
  }

  /**
   * Deletes all expired tokens
   *
   * Note: If you do not plan to use the UsernamePassword provider just provide en empty
   * implementation
   *
   */
  def deleteExpiredTokens() {
    tokens = tokens.filter(!_._2.isExpired)
  }
}
