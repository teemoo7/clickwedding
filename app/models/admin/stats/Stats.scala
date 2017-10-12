package models.admin.stats

import anorm._
import play.api.db.DB
import anorm.SqlParser._
import play.api.Play.current
import java.math.BigDecimal

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 22.08.13
 * Time: 15:40
 * To change this template use File | Settings | File Templates.
 */
case class Stats(generalStats: GeneralStats, dbStats: DBStats, budgetStats: BudgetStats)

object Stats {

  /**
   * Performs all the stats
   */
  def getStats = {
    Stats(GeneralStats.getGeneralStats, DBStats.getDBStats, BudgetStats.getBudgetStats)
  }
}



