package models.admin.stats

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import anorm.~
import java.math.BigDecimal
import play.api.Play.current

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 26.08.13
 * Time: 11:17
 * To change this template use File | Settings | File Templates.
 */
case class DBStats(dbSize: Double, processes: List[DBProcess])
case class DBProcess(id: Pk[Long], user: String, host: String, db: Option[String], command: String, time: Long, state: Option[String], info: Option[String])

object DBStats {

  /**
   * Parse the DB processes list (opened connections)
   */
  val process = {
    get[Pk[Long]]("id") ~
      get[String]("user") ~
      get[String]("host") ~
      get[Option[String]]("db") ~
      get[String]("command") ~
      get[Long]("time") ~
      get[Option[String]]("state") ~
      get[Option[String]]("info") map {
      case id ~ user ~ host ~ db ~ command ~ time ~ state ~ info
      => DBProcess(id, user, host, db, command, time, state, info)
    }
  }

  /**
   * Performs all the DB statistics
   */
  def getDBStats = {
    DBStats(getDBSize, getProcesses)
  }

  /**
   * Get the DB size in MB
   */
  def getDBSize: Double = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT
              Round(Sum(data_length + index_length) / 1024 / 1024, 2) "size"
            FROM information_schema.tables WHERE table_schema <> "information_schema";
          """
        ).as(scalar[BigDecimal].single).doubleValue()
    }
  }

  /**
   * Get processes currently run, i.e. opened connections
   */
  def getProcesses: List[DBProcess] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT id, user, host, db, command, time, state, info FROM information_schema.processlist;
          """
        ).as(process*)
    }
  }
}