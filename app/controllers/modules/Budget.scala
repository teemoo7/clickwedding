package controllers.modules

import controllers.AnyController
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.i18n.Messages
import anorm.{Id, Pk, NotAssigned}
import play.api.mvc.Action
import play.api.Routes
import play.api.libs.json.Json._
import views.html.helper.currency
import play.api.data.format.Formats._
import org.apache.poi.hssf.usermodel.{HeaderFooter, HSSFWorkbook}
import org.apache.poi.ss.util.CellRangeAddress
import org.apache.poi.ss.usermodel.{DataFormat, Font, IndexedColors, CellStyle}
import java.io.ByteArrayOutputStream
import models.modules.{BudgetIncome, BudgetExpense}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 30.01.13
 * Time: 08:42
 * To change this template use File | Settings | File Templates.
 */
object Budget extends AnyController {
  val CURRENCY_CHF = "CHF"
  val CURRENCY_EUR = "€"
  val CURRENCY_USD = "$"
  val CURRENCY_LST = "£"

  val CURRENCIES = Array(CURRENCY_CHF, CURRENCY_EUR, CURRENCY_USD, CURRENCY_LST)

  /**
   * Budget edition form definition
   */
  val budgetInfoForm: Form[models.modules.BudgetInfo] = Form(
    mapping(
      "currency" -> nonEmptyText(maxLength = 3).verifying(Messages("main.modules.budget.errorWrongCurrency"), c => CURRENCIES.contains(c))
    )
      ((currency) => models.modules.BudgetInfo(null, currency, List(), List()))
      ((budgetInfo: models.modules.BudgetInfo) => Some(budgetInfo.currency))
  )

  /**
   * New budget expense form definition
   */
  val addBudgetExpenseForm: Form[models.modules.BudgetExpense] = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "description" -> nonEmptyText(maxLength = 50),
      "price" -> of[Double].verifying(price => price > 0.0)
    )
      (models.modules.BudgetExpense.apply)
      (models.modules.BudgetExpense.unapply)
  )

  /**
   * Edit budget expense form definition
   */
  val editBudgetExpenseForm: Form[models.modules.BudgetExpense] = Form(
    mapping(
      "id" -> number,
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "description" -> nonEmptyText(maxLength = 50),
      "price" -> of[Double].verifying(price => price > 0.0)
    )
      ((id, weddingId, description, price) => models.modules.BudgetExpense(Id(id), weddingId, description, price))
      ((expense: models.modules.BudgetExpense) => Some(expense.id.get.toInt, expense.weddingId, expense.description, expense.price))
  )

  /**
   * Budget expenses sorting
   */
  val budgetExpenseSortForm = Form(
    "expenses" -> list(number)
  )

  /**
   * New budget income form definition
   */
  val addBudgetIncomeForm: Form[models.modules.BudgetIncome] = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "description" -> nonEmptyText(maxLength = 50),
      "price" -> of[Double].verifying(price => price > 0.0)
    )
      (models.modules.BudgetIncome.apply)
      (models.modules.BudgetIncome.unapply)
  )

  /**
   * Edit budget income form definition
   */
  val editBudgetIncomeForm: Form[models.modules.BudgetIncome] = Form(
    mapping(
      "id" -> number,
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "description" -> nonEmptyText(maxLength = 50),
      "price" -> of[Double].verifying(price => price > 0.0)
    )
      ((id, weddingId, description, price) => models.modules.BudgetIncome(Id(id), weddingId, description, price))
      ((income: models.modules.BudgetIncome) => Some(income.id.get.toInt, income.weddingId, income.description, income.price))
  )

  /**
   * Budget incomes sorting
   */
  val budgetIncomeSortForm = Form(
    "incomes" -> list(number)
  )

  /**
   * Define Javascript routes for AJAX calls
   */
  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Budget.addExpense,
        routes.javascript.Budget.editExpense,
        routes.javascript.Budget.deleteExpense,
        routes.javascript.Budget.sortExpenses,
        routes.javascript.Budget.addIncome,
        routes.javascript.Budget.editIncome,
        routes.javascript.Budget.deleteIncome,
        routes.javascript.Budget.sortIncomes
      )
    ).as("text/javascript")
  }

  /**
   * Display the admin console for the budget
   * @param uid the wedding UID
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Budget.loadBudget(wedding.id) match {
              case Some(budget) =>
                Ok(views.html.modules.budget.edit(wedding, budget, addBudgetExpenseForm, budgetInfoForm.fill(budget.moduleContent)))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle edit budget info form submission
   * @param uid the wedding UID
   */
  def doEdit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Budget.loadBudget(wedding.id) match {
              case Some(budget) =>
                budgetInfoForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest(views.html.modules.budget.edit(wedding, budget, addBudgetExpenseForm, formWithErrors)),
                  budgetInfoNew => {
                    models.modules.Budget.editBudget(budget.copy(moduleContent = budget.moduleContent.copy(currency = budgetInfoNew.currency, expenses = List())))
                    goToBudgetEdit(uid).flashing("success" -> Messages("main.modules.budget.modificationsSaved"))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX expense addition form submission
   * @param uid the wedding UID
   */
  def addExpense(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Budget.loadBudget(wedding.id) match {
              case Some(budget) =>
                addBudgetExpenseForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest,
                  expense => {
                    Ok(toJson(Map(
                      "line" -> views.html.modules.budget.editExpense(models.modules.Budget.addBudgetExpense(expense.copy(weddingId = wedding.id)), budget.moduleContent.currency).toString,
                      "total" -> currency(models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString,
                      "result" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum-models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString
                    )))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX expense edition form submission
   * @param uid the wedding UID
   */
  def editExpense(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Budget.loadBudget(wedding.id) match {
              case Some(budget) =>
                editBudgetExpenseForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest,
                  expense => {
                    budget.moduleContent.expenses.map(e => e.id.get).contains(expense.id.get) match {
                      case true =>
                        Ok(toJson(Map(
                          "line" -> views.html.modules.budget.editExpense(models.modules.Budget.editBudgetExpense(expense.copy(weddingId = wedding.id)), budget.moduleContent.currency).toString,
                          "total" -> currency(models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString,
                          "result" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum-models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString
                        )))
                      case false =>
                        NotFound
                    }
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX expense deletion
   * @param uid the wedding UID
   * @param itemId the ID of the deleted item
   */
  def deleteExpense(uid: String, itemId: Long) = SecuredAction(true, IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Budget.loadBudget(wedding.id) match {
              case Some(budget) =>
                budget.moduleContent.expenses.map(e => e.id.get).contains(itemId) match {
                  case true => {
                    models.modules.Budget.deleteBudgetExpense(Id(itemId))
                    Ok(toJson(Map(
                      "total" -> currency(models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString,
                      "result" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum-models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString
                    )))
                  }
                  case false =>
                    NotFound
                }
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX expense sorting form submission
   * @param uid the wedding UID
   */
  def sortExpenses(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Budget.loadBudget(wedding.id) match {
            case Some(budget) =>
              budgetExpenseSortForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest,
                expensesSort => {
                  models.modules.Budget.sortBudgetExpenses(expensesSort)
                  Ok
                }
              )
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Export the budget in an Excel file
   * @param uid the wedding UID
   * @return a binary Excel file
   */
  def exportToExcel(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Budget.loadBudget(wedding.id) match {
            case Some(budget) =>
              Ok(budgetToExcelByteArray(budget.moduleContent, budget.moduleContent.currency)).as("application/vnd.ms-excel").withHeaders(("Content-Disposition", "attachment; filename=\"".concat(Messages("main.modules.budget.budget")).concat(".xls\"")))
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }


  /**
   * Handle AJAX income addition form submission
   * @param uid the wedding UID
   */
  def addIncome(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Budget.loadBudget(wedding.id) match {
            case Some(budget) =>
              addBudgetIncomeForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest,
                income => {
                  Ok(toJson(Map(
                    "line" -> views.html.modules.budget.editIncome(models.modules.Budget.addBudgetIncome(income.copy(weddingId = wedding.id)), budget.moduleContent.currency).toString,
                    "total" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum).toString,
                    "result" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum-models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString
                  )))
                }
              )
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX income edition form submission
   * @param uid the wedding UID
   */
  def editIncome(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Budget.loadBudget(wedding.id) match {
            case Some(budget) =>
              editBudgetIncomeForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest,
                income => {
                  budget.moduleContent.incomes.map(i => i.id.get).contains(income.id.get) match {
                    case true =>
                      Ok(toJson(Map(
                        "line" -> views.html.modules.budget.editIncome(models.modules.Budget.editBudgetIncome(income.copy(weddingId = wedding.id)), budget.moduleContent.currency).toString,
                        "total" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum).toString,
                        "result" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum-models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString
                      )))
                    case false =>
                      NotFound
                  }
                }
              )
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX income deletion
   * @param uid the wedding UID
   * @param itemId the ID of the deleted item
   */
  def deleteIncome(uid: String, itemId: Long) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Budget.loadBudget(wedding.id) match {
            case Some(budget) =>
              budget.moduleContent.incomes.map(i => i.id.get).contains(itemId) match {
                case true => {
                  models.modules.Budget.deleteBudgetIncome(Id(itemId))
                  Ok(toJson(Map(
                    "total" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum).toString,
                    "result" -> currency(models.modules.Budget.getBudgetIncomes(wedding.id).map(i => i.price).sum-models.modules.Budget.getBudgetExpenses(wedding.id).map(e => e.price).sum).toString
                  )))
                }
                case false =>
                  NotFound
              }
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Handle AJAX income sorting form submission
   * @param uid the wedding UID
   */
  def sortIncomes(uid: String) = SecuredAction(true, IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.modules.Budget.loadBudget(wedding.id) match {
            case Some(budget) =>
              budgetIncomeSortForm.bindFromRequest.fold(
                formWithErrors =>
                  BadRequest,
                incomesSort => {
                  models.modules.Budget.sortBudgetIncomes(incomesSort)
                  Ok
                }
              )
            case _ => NotFound
          }
        }
      }.getOrElse(NotFound)
  }

  /**
   * Redirect to the edit form of the budget
   */
  def goToBudgetEdit(uid: String) = Redirect(controllers.modules.routes.Budget.edit(uid))

  protected def budgetToExcelByteArray(budgetInfo: models.modules.BudgetInfo, currency: String): Array[Byte] = {
    val incomes: List[BudgetIncome] = budgetInfo.incomes
    val expenses: List[BudgetExpense] = budgetInfo.expenses

    // Workbook and sheets
    val wb = new HSSFWorkbook()
    val sheetExpenses = wb.createSheet(Messages("main.modules.budget.expenses"))
    val sheetIncomes = wb.createSheet(Messages("main.modules.budget.incomes"))
    val sheetResult = wb.createSheet(Messages("main.modules.budget.result"))

    // Titles
    val titleRowIncomes = sheetIncomes.createRow(0)
    titleRowIncomes.createCell(0).setCellValue(Messages("main.modules.budget.description"))
    titleRowIncomes.createCell(1).setCellValue(Messages("main.modules.budget.price"))
    val titleRowExpenses = sheetExpenses.createRow(0)
    titleRowExpenses.createCell(0).setCellValue(Messages("main.modules.budget.description"))
    titleRowExpenses.createCell(1).setCellValue(Messages("main.modules.budget.price"))

    // Incomes
    val styleCurrency = wb.createCellStyle()
    styleCurrency.setDataFormat(4)
    var lineIncome = 1
    incomes.map {
      income =>
        val row = sheetIncomes.createRow(lineIncome)
        row.createCell(0).setCellValue(income.description)
        val amountCell = row.createCell(1)
        amountCell.setCellValue(income.price)
        amountCell.setCellStyle(styleCurrency)
        lineIncome += 1
    }
    // Expenses
    var lineExpense = 1
    expenses.map {
      expense =>
        val row = sheetExpenses.createRow(lineExpense)
        row.createCell(0).setCellValue(expense.description)
        val amountCell = row.createCell(1)
        amountCell.setCellValue(expense.price)
        amountCell.setCellStyle(styleCurrency)
        lineExpense += 1
    }

    // Total
    val totalRowIncome = sheetIncomes.createRow(lineIncome)
    totalRowIncome.createCell(0).setCellValue(Messages("main.modules.budget.total"))
    val totalAmountIncome = totalRowIncome.createCell(1)
    totalAmountIncome.setCellValue(incomes.map(_.price).sum)
    val totalRowExpense = sheetExpenses.createRow(lineExpense)
    totalRowExpense.createCell(0).setCellValue(Messages("main.modules.budget.total"))
    val totalAmountExpense = totalRowExpense.createCell(1)
    totalAmountExpense.setCellValue(expenses.map(_.price).sum)

    // Result
    val rowIncomes = sheetResult.createRow(0)
    rowIncomes.createCell(0).setCellValue(Messages("main.modules.budget.incomes"))
    val amountCellIncomes = rowIncomes.createCell(1)
    amountCellIncomes.setCellValue(incomes.map(_.price).sum)
    amountCellIncomes.setCellStyle(styleCurrency)
    val rowExpenses = sheetResult.createRow(1)
    rowExpenses.createCell(0).setCellValue(Messages("main.modules.budget.expenses"))
    val amountCellExpenses = rowExpenses.createCell(1)
    amountCellExpenses.setCellValue(expenses.map(_.price).sum)
    amountCellExpenses.setCellStyle(styleCurrency)
    val totalRowResult = sheetResult.createRow(2)
    totalRowResult.createCell(0).setCellValue(Messages("main.modules.budget.total"))
    val totalAmountResult = totalRowResult.createCell(1)
    totalAmountResult.setCellValue(budgetInfo.getResult)


    // Styles
    val styleThinBlack = wb.createCellStyle()
    styleThinBlack.setBorderBottom(CellStyle.BORDER_THIN)
    styleThinBlack.setBottomBorderColor(IndexedColors.BLACK.getIndex)
    styleThinBlack.setBorderLeft(CellStyle.BORDER_THIN)
    styleThinBlack.setLeftBorderColor(IndexedColors.BLACK.getIndex)
    styleThinBlack.setBorderRight(CellStyle.BORDER_THIN)
    styleThinBlack.setRightBorderColor(IndexedColors.BLACK.getIndex)
    styleThinBlack.setBorderTop(CellStyle.BORDER_THIN)
    styleThinBlack.setTopBorderColor(IndexedColors.BLACK.getIndex)
    val fontBold = wb.createFont()
    fontBold.setBoldweight(Font.BOLDWEIGHT_BOLD)
    styleThinBlack.setFont(fontBold)

    val itTitleIncomes = titleRowIncomes.cellIterator
    while (itTitleIncomes.hasNext) {
      val cell = itTitleIncomes.next()
      cell.setCellStyle(styleThinBlack)
    }
    val itTitleExpenses = titleRowExpenses.cellIterator
    while (itTitleExpenses.hasNext) {
      val cell = itTitleExpenses.next()
      cell.setCellStyle(styleThinBlack)
    }

    val itTotalIncome = totalRowIncome.cellIterator
    while (itTotalIncome.hasNext) {
      val cell = itTotalIncome.next()
      cell.setCellStyle(styleThinBlack)
    }
    val itTotalExpense = totalRowExpense.cellIterator
    while (itTotalExpense.hasNext) {
      val cell = itTotalExpense.next()
      cell.setCellStyle(styleThinBlack)
    }

    val itTotalResult = totalRowResult.cellIterator
    while (itTotalResult.hasNext) {
      val cell = itTotalResult.next()
      cell.setCellStyle(styleThinBlack)
    }

    totalAmountIncome.getCellStyle.setDataFormat(4)
    totalAmountExpense.getCellStyle.setDataFormat(4)
    totalAmountResult.getCellStyle.setDataFormat(4)

    // Page setup
    wb.setPrintArea(0, 0, 1, 0, lineIncome.max(lineExpense))
    val footerIncome = sheetIncomes.getFooter
    footerIncome.setRight(Messages("main.modules.budget.page") + " " + HeaderFooter.page + " / " + HeaderFooter.numPages)
    val psIncome = sheetIncomes.getPrintSetup
    sheetIncomes.setAutobreaks(true)
    psIncome.setFitWidth(1)
    psIncome.setFitHeight(0)
    sheetIncomes.autoSizeColumn(0)
    sheetIncomes.autoSizeColumn(1)
    val footerExpense = sheetExpenses.getFooter
    footerExpense.setRight(Messages("main.modules.budget.page") + " " + HeaderFooter.page + " / " + HeaderFooter.numPages)
    val psExpense = sheetExpenses.getPrintSetup
    sheetExpenses.setAutobreaks(true)
    psExpense.setFitWidth(1)
    psExpense.setFitHeight(0)
    sheetExpenses.autoSizeColumn(0)
    sheetExpenses.autoSizeColumn(1)
    val footerResult = sheetResult.getFooter
    footerResult.setRight(Messages("main.modules.budget.page") + " " + HeaderFooter.page + " / " + HeaderFooter.numPages)
    val psResult = sheetResult.getPrintSetup
    sheetResult.setAutobreaks(true)
    psResult.setFitWidth(1)
    psResult.setFitHeight(0)
    sheetResult.autoSizeColumn(0)
    sheetResult.autoSizeColumn(1)

    // Convert the book to an array of bytes
    val outputStream = new ByteArrayOutputStream()
    wb.write(outputStream)
    outputStream.toByteArray
  }
}
