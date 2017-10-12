package models.admin.stats

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import anorm.~

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 26.08.13
 * Time: 11:18
 * To change this template use File | Settings | File Templates.
 */
case class GeneralStats(
                         nbWeddingsTotal: Long,
                         nbEmptyWeddings: Long,
                         averageModulesPerWedding: Double,
                         averageExpensesPerWedding: Double,
                         mostUsedModules: List[ModuleUsage],
                         weddingExpenseRanges: List[WeddingExpenseRange],
                         weddingCreationMonth: List[WeddingCreationMonth])
case class ModuleUsage(moduleId: Pk[Long], usage: Long, price: Long)
case class WeddingExpenseRange(rangeStart: Long, rangeEnd: Long, nb: Long)
case class WeddingCreationMonth(nb: Long, year: Long, month: Long)

object GeneralStats {

  /**
   * Parse the module usage (moduleid, number of times is has been added and the unit price)
   */
  val moduleUsage = {
    get[Pk[Long]]("moduleid") ~
      get[Long]("nb") ~
      get[Long]("price") map {
      case moduleId ~ nb ~ price
      => ModuleUsage(moduleId, nb, price)
    }
  }

  /**
   * Parse the wedding expense ranges
   */
  val weddingExpenseRange = {
    get[java.math.BigDecimal]("rangestart") ~
      get[java.math.BigDecimal]("rangeend") ~
      get[Long]("number") map {
      case rangeStart ~ rangeEnd ~ number
      => WeddingExpenseRange(rangeStart.intValue(), rangeEnd.intValue(), number)
    }
  }

  /**
   * Parse the wedding creation months
   */
  val weddingCreationMonth = {
    get[Long]("nb") ~
      get[Long]("year") ~
      get[Long]("month") map {
      case nb ~ year ~ month
      => WeddingCreationMonth(nb, year, month)
    }
  }

  /**
   * Performs all the general statistics
   */
  def getGeneralStats = {
    GeneralStats(getNbWeddingsTotal, getNbEmptyWeddings, getAverageModulesPerWedding, getAverageExpensesPerWedding, getMostUsedModules, getWeddingExpenseRanges, getWeddingCreationByMonth)
  }

  /**
   * Get the total number of weddings
   */
  def getNbWeddingsTotal: Long = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT count(id) as nb FROM wedding
          """
        ).as(scalar[Long].single)
    }
  }

  /**
   * Get the number of empty weddings (i.e. without any module)
   */
  def getNbEmptyWeddings: Long = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT count(id) as nb
            FROM wedding
            WHERE id NOT IN (
              SELECT DISTINCT weddingid
              FROM weddingmodule
              GROUP BY weddingid
            )
          """
        ).as(scalar[Long].single)
    }
  }

  /**
   * Get the average number of modules per wedding
   */
  def getAverageModulesPerWedding: Double = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT round(avg(nb), 2) as average
            FROM (
              SELECT count(weddingid) AS nb
              FROM weddingmodule
              GROUP BY weddingid
            ) AS counts
          """
        ).as(scalar[java.math.BigDecimal].single).doubleValue()
    }
  }

  /**
   * Get the average amount spent for non-empty weddings
   */
  def getAverageExpensesPerWedding: Double = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT round(avg(total), 2) as average
            FROM (
              SELECT sum(m.price) AS total
              FROM weddingmodule wm, module m
              WHERE wm.moduleid = m.id
              GROUP BY wm.weddingid
            ) AS expenses
          """
        ).as(scalar[java.math.BigDecimal].single).doubleValue()
    }
  }

  /**
   * Get the modules usage, order by most used first
   */
  def getMostUsedModules: List[ModuleUsage] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT  wm.moduleid,
                    count(wm.moduleid) AS nb,
                    m.price
            FROM weddingmodule wm, module m
            WHERE wm.moduleid = m.id
            GROUP BY wm.moduleid
            ORDER BY nb DESC
          """
        ).as(moduleUsage*)
    }
  }

  /**
   * Get the wedding expense ranges
   */
  def getWeddingExpenseRanges: List[WeddingExpenseRange] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT 20 * floor(total / 20) AS `rangestart`,
                   20 * floor(total / 20) + 19 AS `rangeend`,
                   count(*) AS `number`
              FROM (SELECT sum(m.price) AS total
                      FROM weddingmodule wm, module m
                      WHERE wm.moduleid = m.id
                      GROUP BY wm.weddingid) AS expenses
            GROUP BY 1
            ORDER BY total;
          """
        ).as(weddingExpenseRange*)
    }
  }

  /**
   * Get the numbers of new weddings per month
   */
  def getWeddingCreationByMonth: List[WeddingCreationMonth] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            SELECT count(w.id) AS `nb`, YEAR(t.date) AS `year`, MONTH(t.date) AS `month`
            FROM wedding w, money_transaction t
            WHERE w.id = t.weddingid AND t.action = {actionId}
            GROUP BY YEAR(t.date), MONTH(t.date)
            ORDER BY YEAR(t.date), MONTH(t.date);
          """
        ).on(
          'actionId -> controllers.payment.Money.ACTION_OPENING
        ).as(weddingCreationMonth*)
    }
  }
}