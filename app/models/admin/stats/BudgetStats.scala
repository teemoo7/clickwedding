package models.admin.stats

import play.api.db.DB
import anorm._
import anorm.SqlParser._
import play.api.Play.current

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 26.08.13
 * Time: 11:17
 * To change this template use File | Settings | File Templates.
 */
case class BudgetStats(averageBudgetTotalAmount: Double, averageBudgetExpensesNumber: Double)

object BudgetStats {

  /**
   * Performs all the budget statistics
   */
  def getBudgetStats = BudgetStats(getAverageBudgetTotalAmount, getAverageBudgetExpensesNumber)

  /**
   * Get the average budget amount
   */
  def getAverageBudgetTotalAmount: Double = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT round(avg(total), 2)
          FROM (
            SELECT sum(price) AS total
            FROM mod_budget_expense
            GROUP BY weddingid
          ) sums;
          """
        ).as(scalar[java.math.BigDecimal].single).doubleValue()
    }
  }

  /**
   * Get the average budget number of expenses
   */
  def getAverageBudgetExpensesNumber: Double = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT round(avg(nb), 2)
          FROM (
            SELECT count(id) AS nb
            FROM mod_budget_expense
            GROUP BY weddingid
          ) counts;
          """
        ).as(scalar[java.math.BigDecimal].single).doubleValue()
    }
  }
}