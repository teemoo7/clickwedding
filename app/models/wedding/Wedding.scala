package models.wedding

import java.util.{Date, UUID}
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import models.authentication.User
import models.modules.{MainModule, Module}
import java.text.Normalizer

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 25.09.12
 * Time: 15:16
 * To change this template use File | Settings | File Templates.
 */
case class Wedding(id: Pk[Long] = NotAssigned, uid: String, date: Option[Date], place: Option[String], person1: Option[String], person2: Option[String], modules: List[Module], money: Int, code: Option[String]) {
  def getNames: Option[String] = {
    if (person1.isDefined && person2.isDefined) {
      Some(person1.get + " & " + person2.get)
    } else {
      None
    }
  }
}

object Wedding {
  val wedding = {
    get[Pk[Long]]("id") ~
    get[String]("uid") ~
    get[Option[Date]]("date") ~
    get[Option[String]]("place") ~
    get[Option[String]]("person1") ~
    get[Option[String]]("person2") ~
    get[Int]("money") ~
    get[Option[String]]("code") map {
      case id ~ uid ~ date ~ place ~ person1 ~ person2 ~ money ~ code => Wedding(id, uid, date, place, person1, person2, Module.getActiveModulesForWedding(id), money, code)
    }
  }
/**
   * Insert a new wedding
   * @param wedding
   * @param user
   * @return the newly created wedding
   */
  def add(wedding: Wedding, user: User): Wedding = {
    DB.withTransaction {
      implicit connection =>
        val uid = getUniqueId(wedding)
        val id: Option[Long] = SQL(
          """
          INSERT INTO wedding (
            uid, date, place, person1, person2, money, code
          ) VALUES (
            {uid}, {date}, {place}, {person1}, {person2}, 0, {code}
          )
          """
        ).on(
          'uid -> uid,
          'date -> wedding.date,
          'place -> wedding.place,
          'person1 -> wedding.person1,
          'person2 -> wedding.person2,
          'code -> wedding.code
        ).executeInsert()

        val weddingId = id.get

        SQL(
          """
          INSERT INTO weddingowner (
            userid, weddingid
          ) VALUES (
            {userid}, {weddingid}
          )
          """
        ).on(
          'userid -> user.userId,
          'weddingid -> weddingId
        ).executeUpdate()

        val weddingUpdated = wedding.copy(id = Id(weddingId), uid = uid)

        models.modules.Module.addModuleToWeddingWithConnection(weddingUpdated.id, Id(MainModule.ID))
        models.modules.Module.initModuleWithConnection(MainModule.ID, weddingUpdated.id)

        models.payment.Money.addOpening(weddingUpdated, user)

        weddingUpdated
    }
  }

  /**
   * Find all existing weddings
   * @return a list of all weddings
   */
  def all(): List[Wedding] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, uid, date, place, person1, person2, money, code FROM wedding
          """
        ).as(wedding *)
    }
  }

  /**
   * Find all wedding given their owner
   * @param user
   * @return a list a wedding owned by the user
   */
  def findByOwner(user: User): List[Wedding] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT w.id, w.uid, w.date, w.place, w.person1, w.person2, w.money, w.code FROM wedding w, weddingowner wo WHERE
            w.id = wo.weddingid AND
            wo.userid = {userid}
          """
        ).on(
          'userid -> user.userId
        ).as(wedding *)
    }
  }

  /**
   * Find a wedding given its ID
   * @param id
   * @return the wedding if found
   */
  def findById(id: Long): Option[Wedding] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, uid, date, place, person1, person2, money, code FROM wedding WHERE id = {id}
          """
        ).on(
          'id -> id
        ).as(wedding.singleOpt)
    }
  }

  /**
   * Find a wedding given its Unique ID
   * @param uid
   * @return the wedding if found
   */
  def findByUid(uid: String): Option[Wedding] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, uid, date, place, person1, person2, money, code FROM wedding WHERE uid = {uid}
          """
        ).on(
          'uid -> uid
        ).as(wedding.singleOpt)
    }
  }

  /**
   * Find the code of a wedding given its Unique ID
   * @param uid
   * @return the code if found
   */
  def getWeddingCodeByUid(uid: String): Option[String] = {
    DB.withConnection {
      implicit connection =>
        val code: Option[String] = SQL(
          """
          SELECT code FROM wedding WHERE uid = {uid}
          """
        ).on(
          'uid -> uid
        ).as(scalar[Option[String]].single)
        code
      }
  }

  /**
   * Update a wedding
   * @param wedding
   * @return a boolean that indicates if the operation was successful
   */
  def edit(wedding: Wedding) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE wedding SET
           date = {date}, place = {place}, person1 = {person1}, person2 = {person2}, uid = {uid}
          WHERE id = {id}
          """
        ).on(
          'date -> wedding.date,
          'place -> wedding.place,
          'person1 -> wedding.person1,
          'person2 -> wedding.person2,
          'uid -> wedding.uid,
          'id -> wedding.id
        ).executeUpdate()
    }
  }

  /**
   * Update the VIP code of a wedding
   * @param wedding
   * @return a boolean that indicates if the operation was successful
   */
  def editCode(wedding: Wedding) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE wedding SET
           code = {code}
          WHERE id = {id}
          """
        ).on(
          'code -> wedding.code,
          'id -> wedding.id
        ).executeUpdate()
    }
  }

  /**
   * Delete a wedding
   * @param wedding the wedding
   * @return a boolean that indicates if the operation was successful
   */
  def delete(wedding: Wedding) = {
    DB.withTransaction {
      implicit connection =>
        val allModules = models.modules.Module.getAllModulesForWeddingAndInstanceThem(wedding.id)
        allModules.map(module => module.deleteWedding(wedding.id))
        SQL(
          """
          DELETE FROM weddingmodule WHERE weddingid = {id}
          """
        ).on(
          'id -> wedding.id
        ).executeUpdate()
        models.payment.Money.deleteTransactionsForWedding(wedding.id)
        SQL(
          """
          DELETE FROM weddingowner WHERE weddingid = {id}
          """
        ).on(
          'id -> wedding.id
        ).executeUpdate()

        SQL(
          """
          DELETE FROM wedding WHERE id = {id}
          """
        ).on(
          'id -> wedding.id
        ).executeUpdate()
    }
  }

  /**
   * Check if a user is an owner of this wedding
   * @param uid the wedding UID
   * @param userId the user ID
   * @return true if the user is the owner of the wedding
   */
  def isOwner(uid: String, userId: Pk[Long]): Boolean = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT count(wo.userid) FROM wedding w, weddingowner wo WHERE
           w.id = wo.weddingid AND
           wo.userid = {userid} AND
           w.uid = {uid}
          """
        ).on(
          'userid -> userId,
          'uid -> uid
        ).as(scalar[Long].single) > 0
    }
  }

    /**
   * Calculate a unique identifier not used yet in the DB
   */
  protected def getUniqueId(wedding: Wedding): String = {
    var uid: String = ""
    if (wedding.person1.isDefined && wedding.person2.isDefined) {
      val name = wedding.person1.get+"-"+wedding.person2.get
      uid = Normalizer.normalize(name.toLowerCase, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "").replaceAll("\\s","")
      if (uid.length > 20) {
        uid = uid.substring(0, 19)
      }
      if (isUIDAvailable(uid)) {
        return uid
      }
    }
    var isUnique: Boolean = false;
    while (!isUnique) {
      uid = UUID.randomUUID().toString().substring(0, 5)
      isUnique = isUIDAvailable(uid)
    }
    return uid
  }

  /**
   * Test if the given UID is still available or if it is already taken for another wedding
   * @param uid the UID
   * @return true if the UID is still available
   */
  protected def isUIDAvailable(uid: String) = {
    findByUid(uid).isEmpty
  }
}
