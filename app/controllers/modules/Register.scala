package controllers.modules

import play.api.mvc.{Action, Controller}
import controllers.AnyController
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import anorm.{Pk, NotAssigned}
import play.api.i18n.{Lang, Messages}
import models.modules.RegisterGuest
import org.apache.poi.hssf.usermodel.{HeaderFooter, HSSFWorkbook}
import java.io.ByteArrayOutputStream
import org.apache.poi.ss.usermodel.{Font, IndexedColors, CellStyle, Row}
import org.apache.poi.ss.util.CellRangeAddress

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 21.01.13
 * Time: 14:37
 * To change this template use File | Settings | File Templates.
 */
object Register extends AnyController  {

  /**
   * Register options form definition
   */
  val registerOptionsForm = Form(
    mapping (
      "isReception" -> boolean,
      "isDinner" -> boolean,
      "isDinnerPrivate" -> boolean,
      "isNotification" -> boolean,
      "mailNotification" -> optional(email).verifying(Messages("error.maxLength", 150), o => o.getOrElse("").length <= 150)
    )(models.modules.RegisterInfo.apply)(models.modules.RegisterInfo.unapply)
    verifying(Messages("main.modules.register.notificationMailAddressRequired"), registerInfo => !registerInfo.isNotification || registerInfo.notificationMail.isDefined)
  )

  /**
   * Register instructions options form definition
   */
  val registerInstructionsForm = Form(
    "instructions" -> optional(text(minLength = 0, maxLength = 300))
  )

  /**
   * Register guest form definition
   */
  val registerGuestForm = Form(
    mapping (
      "weddingId" -> ignored(NotAssigned:Pk[Long]),
      "firstName" -> optional(text(maxLength = 40)),
      "lastName" -> nonEmptyText(maxLength = 40),
      "isComing" -> nonEmptyText.transform(((t: String) => t.toBoolean), ((b: Boolean) => b.toString)),
      "nbForReception" -> number,
      "nbForDinner" -> number,
      "mailAddress" -> optional(email).verifying(Messages("error.maxLength", 150), o => o.getOrElse("").length <= 150),
      "mobilePhone" -> optional(text(maxLength = 20)),
      "comment" -> optional(text(maxLength = 300))
    )(models.modules.RegisterGuest.apply)(models.modules.RegisterGuest.unapply)
      verifying(Messages("main.modules.register.errorSelectAnOption"), registeredGuest => registeredGuest.isComing == ((registeredGuest.nbForReception+registeredGuest.nbForDinner) > 0))
  )

  /**
   * Display the guest registration form if available
   * @param uid the wedding UID
   */
  def register(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => loadRegisterInfo(wedding).map {
          registerInfo => Ok(views.html.modules.register.register(wedding, registerInfo, registerGuestForm))
        }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Add a new guest registration for the given wedding
   * @param uid the wedding UID
   */
  def doRegister(uid: String) = Action {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => loadRegisterInfo(wedding).map {
          registerInfo =>
            registerGuestForm.bindFromRequest.fold(
              formWithErrors => BadRequest(views.html.modules.register.register(wedding, registerInfo, formWithErrors)).flashing("error" -> Messages("main.modules.register.missingInfo")),
              registerGuest => {
                val nbReception = registerInfo.isReception match {
                  case true => registerGuest.nbForReception
                  case false => 0
                }
                val nbDinner = registerInfo.isDinner match {
                  case true => registerGuest.nbForDinner
                  case false => 0
                }
                val registerGuestComplete = registerGuest.copy(weddingId = wedding.id, nbForReception = nbReception, nbForDinner = nbDinner)
                models.modules.Register.addGuestRegistration(registerGuestComplete)
                controllers.Wedding.goToCurrentWedding(uid).flashing("success" -> Messages("main.modules.register.registrationSuccess"))
              }
            )
        }.getOrElse(NotFound)
      }.getOrElse(NotFound)
  }

  /**
   * Display edit contact info form
   * @param uid the wedding UID
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Register.load(wedding.id) match {
              case Some(register: models.modules.Register) =>
                Ok(views.html.modules.register.edit(wedding, registerOptionsForm.fill(register.moduleContent), registerInstructionsForm.fill(register.instructions)))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle edit form submission
   * @param uid the wedding UID
   */
  def doEdit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Register.load(wedding.id) match {
              case Some(register: models.modules.Register) =>
                registerOptionsForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest(views.html.modules.register.edit(wedding, formWithErrors, registerInstructionsForm.fill(register.instructions))),
                  registerInfo => {
                    models.modules.Register.editRegisterInfo(register.copy(moduleContent = registerInfo))
                    controllers.Wedding.goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.modules.register.modificationsSaved"))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Handle edit instructions form submission
   * @param uid the wedding UID
   */
  def doEditInstructions(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Register.load(wedding.id) match {
              case Some(register: models.modules.Register) =>
                registerInstructionsForm.bindFromRequest.fold(
                  formWithErrors =>
                    BadRequest(views.html.modules.register.edit(wedding, registerOptionsForm.fill(register.moduleContent), formWithErrors)),
                  instructions => {
                    models.modules.Register.editRegisterInstructions(register.copy(instructions = instructions))
                    controllers.Wedding.goToCurrentWedding(uid).flashing("success" -> Messages("main.modules.register.modificationsSaved"))
                  }
                )
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Display the list of registered guests for the given wedding
   * @param uid the wedding UID
   */
  def list(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Register.load(wedding.id) match {
              case Some(register: models.modules.Register) =>
                val guests = models.modules.Register.getRegisteredGuests(wedding.id)
                Ok(views.html.modules.register.list(wedding, guests, register.moduleContent))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }

  /**
   * Export the list of registered guests in an Excel file
   * @param uid the wedding UID
   * @return a binary Excel file
   */
  def exportToExcel(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
      implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
          wedding => {
            models.modules.Register.load(wedding.id) match {
              case Some(register: models.modules.Register) =>
                val guests = models.modules.Register.getRegisteredGuests(wedding.id)
                Ok(guestsToExcelByteArray(guests)).as("application/vnd.ms-excel").withHeaders(("Content-Disposition", "attachment; filename=\"".concat(Messages("main.modules.register.registration")).concat(".xls\"")))
              case _ => NotFound
            }
          }
        }.getOrElse(NotFound)
  }


  /**
   * Load the registration module given a wedding
   * @param wedding
   * @return the registration module if found
   */
  protected def loadRegisterInfo(wedding: models.wedding.Wedding): Option[models.modules.RegisterInfo] = {
    wedding.modules.find(module => module.getId.get == models.modules.Register.ID) match {
      case Some(module) =>
        module match {
          case register: models.modules.Register =>
            Some(register.moduleContent)
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  protected def guestsToExcelByteArray(guests: List[RegisterGuest])(implicit lang: Lang): Array[Byte] = {
    // Workbook and sheet
    val wb = new HSSFWorkbook()
    val sheet = wb.createSheet(Messages("main.modules.register.registeredGuests2"))
    val sheetApo = wb.createSheet(Messages("main.modules.register.apologizedGuests"))

    // Titles
    val titleRow = sheet.createRow(0)
    titleRow.createCell(0).setCellValue(Messages("main.modules.register.firstName"))
    titleRow.createCell(1).setCellValue(Messages("main.modules.register.lastName"))
    titleRow.createCell(2).setCellValue(Messages("main.modules.register.reception"))
    titleRow.createCell(3).setCellValue(Messages("main.modules.register.dinner"))
    titleRow.createCell(4).setCellValue(Messages("main.modules.register.mail"))
    titleRow.createCell(5).setCellValue(Messages("main.modules.register.mobile"))
    titleRow.createCell(6).setCellValue(Messages("main.modules.register.comment"))

    val titleRowApo = sheetApo.createRow(0)
    titleRowApo.createCell(0).setCellValue(Messages("main.modules.register.firstName"))
    titleRowApo.createCell(1).setCellValue(Messages("main.modules.register.lastName"))
    titleRowApo.createCell(2).setCellValue(Messages("main.modules.register.mail"))
    titleRowApo.createCell(3).setCellValue(Messages("main.modules.register.mobile"))
    titleRowApo.createCell(4).setCellValue(Messages("main.modules.register.comment"))

    // Guests
    var line = 1
    guests.filter(_.isComing).map {
      guest =>
        val row = sheet.createRow(line)
        row.createCell(0).setCellValue(guest.firstName.getOrElse(""))
        row.createCell(1).setCellValue(guest.lastName)
        row.createCell(2).setCellValue(guest.nbForReception)
        row.createCell(3).setCellValue(guest.nbForDinner)
        row.createCell(4).setCellValue(guest.mailAddress.getOrElse(""))
        row.createCell(5).setCellValue(guest.mobilePhone.getOrElse(""))
        row.createCell(6).setCellValue(guest.comment.getOrElse(""))
        line += 1
    }

    var lineApo = 1
    guests.filterNot(_.isComing).map {
      guest =>
        val row = sheetApo.createRow(lineApo)
        row.createCell(0).setCellValue(guest.firstName.getOrElse(""))
        row.createCell(1).setCellValue(guest.lastName)
        row.createCell(2).setCellValue(guest.mailAddress.getOrElse(""))
        row.createCell(3).setCellValue(guest.mobilePhone.getOrElse(""))
        row.createCell(4).setCellValue(guest.comment.getOrElse(""))
        lineApo += 1
    }

    // Total
    val totalRow = sheet.createRow(line)
    totalRow.createCell(0).setCellValue(Messages("main.modules.register.total"))
    totalRow.createCell(1).setCellValue("")
    totalRow.createCell(2).setCellValue(guests.map(_.nbForReception).sum)
    totalRow.createCell(3).setCellValue(guests.map(_.nbForDinner).sum)
    totalRow.createCell(4).setCellValue("")
    totalRow.createCell(5).setCellValue("")
    totalRow.createCell(6).setCellValue("")
    sheet.addMergedRegion(new CellRangeAddress(line, line, 0, 1))
    sheet.addMergedRegion(new CellRangeAddress(line, line, 4, 6))

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

    val itTitle = titleRow.cellIterator
    while (itTitle.hasNext) {
      val cell = itTitle.next()
      cell.setCellStyle(styleThinBlack)
    }
    val itTitleApo = titleRowApo.cellIterator
    while (itTitleApo.hasNext) {
      val cell = itTitleApo.next()
      cell.setCellStyle(styleThinBlack)
    }

    val itTotal = totalRow.cellIterator
    while (itTotal.hasNext) {
      val cell = itTotal.next()
      cell.setCellStyle(styleThinBlack)
    }

    // Page setup
    wb.setPrintArea(0, 0, 6, 0, line)
    val footer = sheet.getFooter
    footer.setRight(Messages("main.modules.register.page") + " " + HeaderFooter.page + " / " + HeaderFooter.numPages)
    val ps = sheet.getPrintSetup
    sheet.setAutobreaks(true)
    ps.setFitWidth(1)
    ps.setFitHeight(0)
    sheet.autoSizeColumn(0)
    sheet.autoSizeColumn(1)
    sheet.autoSizeColumn(2)
    sheet.autoSizeColumn(3)
    sheet.autoSizeColumn(4)
    sheet.autoSizeColumn(5)
    sheet.autoSizeColumn(6)
    val footerApo = sheetApo.getFooter
    footerApo.setRight(Messages("main.modules.register.page") + " " + HeaderFooter.page + " / " + HeaderFooter.numPages)
    val psApo = sheetApo.getPrintSetup
    sheetApo.setAutobreaks(true)
    psApo.setFitWidth(1)
    psApo.setFitHeight(0)
    sheetApo.autoSizeColumn(0)
    sheetApo.autoSizeColumn(1)
    sheetApo.autoSizeColumn(2)
    sheetApo.autoSizeColumn(3)
    sheetApo.autoSizeColumn(4)

    // Convert the book to an array of bytes
    val outputStream = new ByteArrayOutputStream()
    wb.write(outputStream)
    outputStream.toByteArray
  }
}
