package models.authentication

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import java.util.Date
import securesocial.core._
import providers.UsernamePasswordProvider
import securesocial.core.UserId
import anorm.~
import securesocial.core.PasswordInfo
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 05.10.12
 * Time: 15:45
 * To change this template use File | Settings | File Templates.
 */
case class User(userId: Pk[Long], id: UserId, firstName: String, lastName: String, fullName: String, email: Option[String],
                 avatarUrl: Option[String], lastLogin: Option[Date], admin : Boolean,
                 authMethod: AuthenticationMethod,
                 oAuth1Info: Option[OAuth1Info] = None,
                 oAuth2Info: Option[OAuth2Info] = None,
                 passwordInfo: Option[PasswordInfo] = None) extends Identity

object User {
  def apply(i: Identity): User = {
    User(null,i.id, i.firstName, i.lastName, i.fullName,
      i.email, i.avatarUrl, None, false, i.authMethod, i.oAuth1Info,
      i.oAuth2Info, i.passwordInfo
    )
  }

  /**
   * Parse a social user from a ResultSet
   */
  val socialUser = {
    get[Pk[Long]]("user.id") ~
    get[Option[String]]("user.email") ~
    get[Option[Date]]("user.lastlogin") ~
    get[Option[String]]("user.avatar") ~
    get[String]("user.firstname") ~
    get[String]("user.lastname") ~
    get[String]("user.fullname") ~
    get[Boolean]("user.admin") ~
    get[String]("socialuser.id") ~
    get[String]("socialuser.providerid") ~
    get[String]("socialuser.method") ~
    get[Option[String]]("socialuser.password") ~
    get[Option[String]]("socialuser.hasher") ~
    get[Option[String]]("socialuser.salt") map {
      case id ~ email ~ lastLogin ~ avatar ~ firstName ~ lastName ~ fullName ~ admin ~ socialId ~ providerId ~ method ~ password ~ hasher ~ salt =>
        User(id, UserId(socialId, providerId), firstName, lastName, fullName, email, avatar, lastLogin, admin, method match {
          case "oauth1" => AuthenticationMethod.OAuth1
          case "oauth2" => AuthenticationMethod.OAuth2
          case "openId" => AuthenticationMethod.OpenId
          case _ => AuthenticationMethod.UserPassword
        },
          passwordInfo = (hasher, password, salt) match {
            case (Some(h), Some(p), s) => Some(PasswordInfo(h, p, s))
            case _ => None
          }
        )
    }
  }

  /**
   * Find and retrieve a user given its ID
   * @param id
   * @return a User if found
   */
  def findById(id: Long): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT
         user.id, user.email, user.lastlogin, user.avatar, user.firstname, user.lastname, user.fullname, user.admin, socialuser.id, socialuser.providerid, socialuser.method, socialuser.password, socialuser.hasher, socialuser.salt
        FROM user, socialuser WHERE
         user.id = {id} AND
         user.id = socialuser.userid
        """
      ).on(
        'id -> id
      ).as(socialUser.singleOpt)
    }
  }

  /**
   * Find and retrieve a user given its mail address
   * @param email
   * @return a User if found
   */
  def findByEmail(email: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT
          user.id, user.email, user.lastlogin, user.avatar, user.firstname, user.lastname, user.fullname, user.admin, socialuser.id, socialuser.providerid, socialuser.method, socialuser.password, socialuser.hasher, socialuser.salt
        FROM user, socialuser WHERE
          user.id = socialuser.userid AND
          socialuser.id = {id} AND
          socialuser.providerid = {providerid}
        """
      ).on(
        'id -> email,
        'providerid -> UsernamePasswordProvider.UsernamePassword
      ).as(socialUser.singleOpt)
    }
  }

  /**
   * Find and retrieve a social user given its social id
   * @param userId social user id
   * @return a social user if found
   */
  def findBySocialUserId(userId: securesocial.core.UserId): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT
          user.id, user.email, user.lastlogin, user.avatar, user.firstname, user.lastname, user.fullname, user.admin, socialuser.id, socialuser.providerid, socialuser.method, socialuser.password, socialuser.hasher, socialuser.salt
        FROM user, socialuser
        WHERE
         user.id = socialuser.userid AND
         socialuser.id = {id} AND
         socialuser.providerid = {providerid}
        """
      ).on(
        'id -> userId.id,
        'providerid -> userId.providerId
      ).as(socialUser.singleOpt)
    }
  }

  /**
   * Insert a new user
   * @param identity the social user identity
   * @return the newly created social user
   */
  def createUser(identity: Identity): Option[Identity] = {
    DB.withTransaction {
      implicit connection =>
        val id: Option[Long] = SQL(
          """
          INSERT INTO user (
           email, lastlogin, avatar, firstname, lastname, fullname, admin
          ) VALUES (
           {email}, NOW(), {avatar}, {firstname}, {lastname}, {fullname}, {admin}
          )
          """
        ).on(
          'email -> identity.email,
          'avatar -> identity.avatarUrl,
          'firstname -> identity.firstName,
          'lastname -> identity.lastName,
          'fullname -> identity.fullName,
          'admin -> false
        ).executeInsert()

        val userId = id.get

        val password: Option[String] = identity.passwordInfo match {
          case Some(passwordInfo) => Some(passwordInfo.password)
          case None => None
        }
        val hasher: Option[String] = identity.passwordInfo match {
          case Some(passwordInfo) => Some(passwordInfo.hasher)
          case None => None
        }
        val salt: Option[String] = identity.passwordInfo match {
          case Some(passwordInfo) => passwordInfo.salt
          case None => None
        }

        SQL(
          """
          INSERT INTO socialuser (
           userid, id, providerid, method, password, hasher, salt
          ) VALUES (
           {userid}, {id}, {providerid}, {method}, {password}, {hasher}, {salt}
          )
          """
        ).on(
          'userid -> userId,
          'id -> identity.id.id,
          'providerid -> identity.id.providerId,
          'method -> identity.authMethod.method,
          'password -> password,
          'hasher -> hasher,
          'salt -> salt
        ).executeUpdate()
        userId
    }
    User.findBySocialUserId(identity.id)

  }

  /**
   * Update a user
   * @param user the new user version
   */
  def updateUser(user: User) = {
    DB.withTransaction {
      implicit connection =>
        SQL(
          """
          UPDATE user SET
            email = {email}, lastlogin = NOW(), avatar = {avatar}, firstname = {firstname}, lastname = {lastname}, fullname = {fullname}
          WHERE id = {id}
          """
        ).on(
          'email -> user.email,
          'avatar -> user.avatarUrl,
          'firstname -> user.firstName,
          'lastname -> user.lastName,
          'fullname -> user.fullName,
          'id -> user.userId
        ).executeUpdate()

        val password: Option[String] = user.passwordInfo match {
          case Some(passwordInfo) => Some(passwordInfo.password)
          case None => None
        }
        val hasher: Option[String] = user.passwordInfo match {
          case Some(passwordInfo) => Some(passwordInfo.hasher)
          case None => None
        }
        val salt: Option[String] = user.passwordInfo match {
          case Some(passwordInfo) => passwordInfo.salt
          case None => None
        }

        SQL(
          """
          UPDATE socialuser SET
           password = {password}, hasher = {hasher}, salt = {salt}
          WHERE userid = {userid}

          """
        ).on(
          'password -> password,
          'hasher -> hasher,
          'salt -> salt,
          'userid -> user.userId
      ).executeUpdate()
      user
    }
  }

}
