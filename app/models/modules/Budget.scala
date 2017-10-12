package models.modules

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import anorm.~
import anorm.Id
import play.api.Play.current
import java.sql.Connection
import play.api.i18n.{Lang, Messages}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 27.02.13
 * Time: 09:40
 * To change this template use File | Settings | File Templates.
 */
case class Budget(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: BudgetInfo, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SMALL_ONLY, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_LEFT)), displayPreviousModuleId, Module.DISPLAY_LEVEL_OWNER) {
  override def init()(implicit connection: Connection) = {
    Budget.addBudget(this)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    Budget.deleteModuleForWedding(weddingId, this)
  }
  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] =
    Some(Messages("main.modules.budget.budget"))
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    views.html.modules.budget.displaySmall(wedding)
  }
}

case class BudgetInfo(weddingId: Pk[Long], currency: String, expenses: List[BudgetExpense], incomes: List[BudgetIncome]) {
  def getResult: Double = incomes.map(_.price).sum - expenses.map(_.price).sum
}

case class BudgetExpense(id: Pk[Long], weddingId: Pk[Long], description: String, price: Double)

case class BudgetIncome(id: Pk[Long], weddingId: Pk[Long], description: String, price: Double)


object Budget {

  val ID = 7
  val NAME = "Budget"

  /**
   * Parse budget info and its expenses and incomes from a ResultSet
   */
  val budget = {
    get[Pk[Long]]("weddingid") ~
      get[String]("currency") ~
      get[Boolean]("active") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") map {
      case weddingId ~ currency ~ active ~ displayColumn ~ displayPreviousModuleId
        => Budget(weddingId, Id(Budget.ID), Budget.NAME, BudgetInfo(weddingId, currency, getBudgetExpenses(weddingId), getBudgetIncomes(weddingId)), active, None, displayColumn, displayPreviousModuleId)
    }
  }

  /**
   * Parse budget expenses from a ResultSet
   */
  val budgetExpense = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingid") ~
      get[String]("description") ~
      get[Double]("price") map {
      case id ~ weddingId ~ description ~ price
        => BudgetExpense(id, weddingId, description, price)
    }
  }

  /**
   * Parse budget incomes from a ResultSet
   */
  val budgetIncome = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingid") ~
      get[String]("description") ~
      get[Double]("price") map {
      case id ~ weddingId ~ description ~ price
        => BudgetIncome(id, weddingId, description, price)
    }
  }

  /**
   * Insert a new budget for the given wedding
   * @param budget the initial value for the budget
   */
  def addBudget(budget: Budget)(implicit connection: Connection) = {
    SQL(
      """
        INSERT INTO mod_budget (
          weddingid, currency
        ) VALUES (
          {weddingid}, {currency}
        )
      """
    ).on(
      'weddingid -> budget.weddingId,
      'currency -> budget.moduleContent.currency
    ).executeUpdate()
  }

  /**
   * Update the budget info for a given wedding
   * @param budget the budget
   */
  def editBudget(budget: Budget) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_budget SET
              currency = {currency}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> budget.weddingId,
          'currency -> budget.moduleContent.currency
        ).executeUpdate()
    }
  }

  /**
   * Load the budget of a wedding
   * @param weddingId the ID of the wedding
   * @return the budget if it exists
   */
  def loadBudget(weddingId: Pk[Long]): Option[Budget] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT b.weddingid, b.currency, wm.active, wm.displaycolumn, wm.displayprevious
        FROM mod_budget b, weddingmodule wm
        WHERE
          b.weddingid = {weddingid} AND
          wm.weddingid = b.weddingid AND
          wm.moduleid = {moduleid}
        """
      ).on(
        'weddingid -> weddingId,
        'moduleid -> Budget.ID
      ).as(budget.singleOpt)
    }
  }

  /**
   * Return the list of all budget expenses for the wedding
   * @param weddingId the ID of the wedding
   * @return a list of expenses
   */
  def getBudgetExpenses(weddingId: Pk[Long]): List[BudgetExpense] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT e.id, e.weddingid, e.description, e.price
          FROM mod_budget_expense e LEFT OUTER JOIN mod_budget_expense_sort s ON e.id = s.id
          WHERE
            e.weddingid = {weddingid}
          ORDER BY s.pos ASC, e.id ASC
          """
        ).on(
          'weddingid -> weddingId
        ).as(budgetExpense *)
    }
  }

  /**
   * Return the list of all budget incomes for the wedding
   * @param weddingId the ID of the wedding
   * @return a list of incomes
   */
  def getBudgetIncomes(weddingId: Pk[Long]): List[BudgetIncome] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT i.id, i.weddingid, i.description, i.price
          FROM mod_budget_income i LEFT OUTER JOIN mod_budget_income_sort s ON i.id = s.id
          WHERE
            i.weddingid = {weddingid}
          ORDER BY s.pos ASC, i.id ASC
          """
        ).on(
          'weddingid -> weddingId
        ).as(budgetIncome *)
    }
  }

  /**
   * Insert a new budget expense for the given wedding
   * @param expense the value for the new expense
   */
  def addBudgetExpense(expense: BudgetExpense) = {
    DB.withTransaction {
      implicit connection =>
        val id: Option[Long] = SQL(
          """
          INSERT INTO mod_budget_expense (
            weddingid, description, price
          ) VALUES (
            {weddingid}, {description}, {price}
          )
          """
        ).on(
          'weddingid -> expense.weddingId,
          'description -> expense.description,
          'price -> expense.price
        ).executeInsert()

        addBudgetExpenseSort(id.get.toInt, expense.weddingId)

        expense.copy(id = Id(id.get))
    }
  }

  /**
   * Update the budget expense
   * @param expense the expense
   */
  def editBudgetExpense(expense: BudgetExpense) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_budget_expense SET
              description = {description}, price = {price}
            WHERE
              id = {id}
          """
        ).on(
          'description -> expense.description,
          'price -> expense.price,
          'id -> expense.id
        ).executeUpdate()

        expense
    }
  }

  /**
   * Delete a budget expense
   * @param id the id of the expense
   */
  def deleteBudgetExpense(id: Pk[Long]) = {
    DB.withTransaction{
      implicit connection =>
        deleteBudgetExpenseSort(id.get.toInt)
        SQL(
          """
          DELETE FROM mod_budget_expense
          WHERE
           id = {id}
          """
        ).on(
          'id -> id
        ).executeUpdate()
    }
  }

  /**
   * Insert a new budget income for the given wedding
   * @param income the value for the new income
   */
  def addBudgetIncome(income: BudgetIncome) = {
    DB.withTransaction {
      implicit connection =>
        val id: Option[Long] = SQL(
          """
          INSERT INTO mod_budget_income (
            weddingid, description, price
          ) VALUES (
            {weddingid}, {description}, {price}
          )
          """
        ).on(
          'weddingid -> income.weddingId,
          'description -> income.description,
          'price -> income.price
        ).executeInsert()

        addBudgetIncomeSort(id.get.toInt, income.weddingId)

        income.copy(id = Id(id.get))
    }
  }

  /**
   * Update the budget income
   * @param income the income
   */
  def editBudgetIncome(income: BudgetIncome) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_budget_income SET
              description = {description}, price = {price}
            WHERE
              id = {id}
          """
        ).on(
          'description -> income.description,
          'price -> income.price,
          'id -> income.id
        ).executeUpdate()

        income
    }
  }

  /**
   * Delete a budget income
   * @param id the id of the income
   */
  def deleteBudgetIncome(id: Pk[Long]) = {
    DB.withTransaction{
      implicit connection =>
        deleteBudgetIncomeSort(id.get.toInt)
        SQL(
          """
          DELETE FROM mod_budget_income
          WHERE
           id = {id}
          """
        ).on(
          'id -> id
        ).executeUpdate()
    }
  }

  /**
   * Delete all info of the budget for the given wedding
   * @param weddingId the wedding ID
   * @param budget the budget info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], budget: Budget)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_budget_expense_sort
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_budget_expense
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_budget_income_sort
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_budget_income
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_budget
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }

  /**
   * Update all the budget expenses positions in the given sorted order
   * @param expenses
   */
  def sortBudgetExpenses(expenses: List[Int]) = {
    DB.withTransaction {
      implicit connection =>
        expenses.zipWithIndex.map { case (expenseId, index) =>
          updateBudgetExpenseSort(expenseId, index)
        }
    }
  }

  /**
   * Update the position of an expense (identified by its id)
   */
  protected def updateBudgetExpenseSort(expenseId: Int, pos: Int)(implicit connection: Connection) = {
    SQL(
      """
      UPDATE mod_budget_expense_sort SET
        pos = {pos}
      WHERE
        id = {id}
      """
    ).on(
      'pos -> pos,
      'id -> expenseId
    ).executeUpdate()
  }

  /**
   * Insert the position of an expense (identified by its id). Position is the highest position found + 1, or 0 if it is the first expense
   */
  protected def addBudgetExpenseSort(expenseId: Int, weddingId: Pk[Long])(implicit connection: Connection) = {
    SQL(
      """
      INSERT INTO mod_budget_expense_sort (
        id, weddingid, pos
      ) VALUES (
        {id}, {weddingid}, (SELECT IFNULL(max(s.pos)+1, 0) FROM mod_budget_expense_sort s)
      )
      """
    ).on(
      'id -> expenseId,
      'weddingid -> weddingId
    ).executeUpdate()
  }

  /**
   * Delete the position of an expense (identified by its id)
   */
  protected def deleteBudgetExpenseSort(expenseId: Int)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_budget_expense_sort
      WHERE
        id = {id}
      """
    ).on(
      'id -> expenseId
    ).executeUpdate()
  }

  /**
   * Update all the budget incomes positions in the given sorted order
   * @param incomes
   */
  def sortBudgetIncomes(incomes: List[Int]) = {
    DB.withTransaction {
      implicit connection =>
        incomes.zipWithIndex.map { case (incomeId, index) =>
          updateBudgetIncomeSort(incomeId, index)
        }
    }
  }

  /**
   * Update the position of an income (identified by its id)
   */
  protected def updateBudgetIncomeSort(incomeId: Int, pos: Int)(implicit connection: Connection) = {
    SQL(
      """
      UPDATE mod_budget_income_sort SET
        pos = {pos}
      WHERE
        id = {id}
      """
    ).on(
      'pos -> pos,
      'id -> incomeId
    ).executeUpdate()
  }

  /**
   * Insert the position of an income (identified by its id). Position is the highest position found + 1, or 0 if it is the first income
   */
  protected def addBudgetIncomeSort(incomeId: Int, weddingId: Pk[Long])(implicit connection: Connection) = {
    SQL(
      """
      INSERT INTO mod_budget_income_sort (
        id, weddingid, pos
      ) VALUES (
        {id}, {weddingid}, (SELECT IFNULL(max(s.pos)+1, 0) FROM mod_budget_income_sort s)
      )
      """
    ).on(
      'id -> incomeId,
      'weddingid -> weddingId
    ).executeUpdate()
  }

  /**
   * Delete the position of an income (identified by its id)
   */
  protected def deleteBudgetIncomeSort(incomeId: Int)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_budget_income_sort
      WHERE
        id = {id}
      """
    ).on(
      'id -> incomeId
    ).executeUpdate()
  }

}