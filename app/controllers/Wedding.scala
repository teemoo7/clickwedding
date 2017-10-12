package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import anorm.{Pk, NotAssigned}
import play.api.i18n.Messages
import models.authentication.User
import controllers.admin.NotificationCenter

object Wedding extends AnyController {

  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Wedding.index)

  /**
   * Search wedding form definition
   */
  val searchPublicWeddingForm = Form(
    tuple(
      "uid" -> nonEmptyText,
      "code" -> optional(text)
    )
//      "captchaUuid" -> nonEmptyText,
//      "captchaValue" -> nonEmptyText
//    ).verifying(Messages("main.captcha.captchaDoesNotMatch"), form => form match {
//      case (uid, code, captchaUuid, captchaValue) => validateCaptcha(captchaValue, captchaUuid)
//    })
  )

  /**
   * Wedding creation form definition
   */
  val createWeddingForm = Form(
    mapping (
      "date" -> optional(date("dd.MM.yyyy")),
      "place" -> optional(text(maxLength = 20)),
      "person1" -> optional(text(maxLength = 30)),
      "person2" -> optional(text(maxLength = 30)),
      "accept" -> checked(Messages("main.wedding.pleaseAcceptTermsAndConditions"))
    )
    (
      (date, place, person1, person2, _) => models.wedding.Wedding(null, null, date, place, person1, person2, List(), 0, None)
    )
    (
      (wedding: models.wedding.Wedding) => Some(wedding.date, wedding.place, wedding.person1, wedding.person2, false)
    )
  )

  /**
   * Wedding form definition
   */
  val weddingForm = Form(
    mapping (
      "id" -> ignored(NotAssigned:Pk[Long]),
      "uid" -> text(maxLength = 20, minLength = 5),
      "date" -> optional(date("dd.MM.yyyy")),
      "place" -> optional(text(maxLength = 20)),
      "person1" -> optional(text(maxLength = 30)),
      "person2" -> optional(text(maxLength = 30))
    )
    (
      (_, uid, date, place, person1, person2) => models.wedding.Wedding(null, uid, date, place, person1, person2, null, 0, None)
    )
    (
      (wedding: models.wedding.Wedding) => Some(wedding.id, wedding.uid, wedding.date, wedding.place, wedding.person1, wedding.person2)
    )
  )

  /**
   * Wedding code form definition
   */
  val weddingCodeForm = Form(
    "code" -> optional(text)
  )

  /**
   * Wedding change UID form definition
   */
  val weddingUidForm = Form(
    tuple(
      "uid" -> nonEmptyText,
      "newUid" -> nonEmptyText(5, 20).verifying(Messages("main.wedding.identifierInvalid"), newUid => newUid.matches("""([a-z0-9_\\-]*)"""))
    ) verifying(Messages("main.wedding.identifierAlreadyTaken"), form => form match {
      case(uid, newUid) => (!newUid.equalsIgnoreCase(uid)) && models.wedding.Wedding.findByUid(newUid).isEmpty
    })
  )

  /**
   * Index
   */
  def index = UserAwareAction {
    implicit request =>
      request.user match {
        case Some(identity) => Redirect(routes.Wedding.list)
        case _ => Redirect(routes.Wedding.welcome)
      }
  }

  /**
   * Welcome homepage
   */
  def welcome = Action {
    implicit request =>
      Ok(views.html.wedding.index(securesocial.core.providers.UsernamePasswordProvider.loginForm))
  }

  /**
   * Display an empty search form
   */
  def search = Action {
    implicit request =>
      Ok(views.html.wedding.search(searchPublicWeddingForm))
  }

  /**
   * Display a pre-filled search form
   */
  def code(uid: String) = Action {
    implicit request =>
      Ok(views.html.wedding.code(searchPublicWeddingForm.fill((uid, None))))
  }

  /**
   * Handle search form submission
   */
  def doSearch = Action {
    implicit request => searchPublicWeddingForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.wedding.search(formWithErrors))
      },
      {
        case (data) => models.wedding.Wedding.findByUid(data._1) match {
          case Some(wedding) => {
            val enteredCode = data._2
            wedding.code match {
              case Some(weddingCode) => {
                enteredCode match {
                  case Some(code) => {
                    setCode(code)
                    weddingCode.equalsIgnoreCase(code) match {
                      case true => {
                        goToCurrentWedding(wedding.uid).flashing("success" -> Messages("main.wedding.validWeddingCode"))
                      }
                      case false => {
                        goToCurrentWedding(wedding.uid).flashing("error" -> Messages("main.wedding.wrongWeddingCode"))
                      }
                    }
                  }
                  case None =>
                    removeCode
                    goToCurrentWedding(wedding.uid)
                }
              }
              case None =>
                goToCurrentWedding(wedding.uid)
            }
          }
          case None => {
            BadRequest(views.html.wedding.search(searchPublicWeddingForm.fill(data).withGlobalError(Messages("main.wedding.notFound", data._1))))
          }
        }
      }
    )
  }

  /**
   * Display a wedding given its UID
   * @param uid
   */
  def display(uid: String) = UserAwareAction {
    implicit request =>
        models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          implicit val isOwnerOfWedding: Boolean =
            request.user match {
              case Some(user) => this.isOwnerOfWedding(uid)(user.asInstanceOf[models.authentication.User])
              case None => false
            }
          val isTutorial = isOwnerOfWedding match {
            case true => request.request.queryString.get(helpers.TutorialHelper.forceTutorialParameter).isDefined
            case false => false
          }
          Ok(views.html.wedding.display(wedding, isTutorial))
        }
      }.getOrElse(NotFound)
  }

  /**
   * Display the list of all weddings (admin only)
   */
  def listAll = SecuredAction(IsAdministrator()) {
    implicit request =>
      Ok(views.html.wedding.list(models.wedding.Wedding.all()))
  }

  /**
   * Display the list the user's weddings if several weddings exist, redirect to the only wedding or to the creation form
   */
  def list = SecuredAction {
    implicit request =>
      if (request.user.asInstanceOf[models.authentication.User].admin) {
        Redirect(controllers.admin.routes.Console.display)
      } else {
        val weddingList = models.wedding.Wedding.findByOwner(request.user.asInstanceOf[models.authentication.User])
        weddingList.size match {
          case 0 => {
            val providerList = models.providers.Provider.getProvidersByUserId(request.user.asInstanceOf[models.authentication.User])
            providerList.size match {
              case 0 => Redirect(routes.Wedding.create)
              case _ => Redirect(controllers.providers.routes.Provider.list)
            }
          }
          case 1 => Redirect(routes.Wedding.display(weddingList(0).uid))
          case _ => Ok(views.html.wedding.list(weddingList))
        }
      }
  }

  def example = Action {
    implicit request =>
      val idExampleWedding = play.Play.application.configuration.getLong("wedding.example.id")
      val exampleWedding = models.wedding.Wedding.findById(idExampleWedding)
      exampleWedding match {
        case Some(wedding) =>
          Redirect(routes.Wedding.display(wedding.uid))
        case None =>
          Redirect(routes.Wedding.index)
      }
  }

  /**
   * Display an empty wedding form
   */
  def create = SecuredAction {
    implicit request =>
      Ok(views.html.wedding.create(createWeddingForm))
  }

  /**
   * Handle new wedding form submission
   */
  def doCreate = SecuredAction {
    implicit request => createWeddingForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.wedding.create(formWithErrors)).flashing("error" -> Messages("main.wedding.missingInfo")),
      wedding => {
        val weddingCreated = models.wedding.Wedding.add(wedding, request.user.asInstanceOf[User])
        val view = views.html.admin.notification.mailNotificationWeddingCreated(weddingCreated)
        val message = view.body
        NotificationCenter.adminNotificationMail("[INFO] Nouveau mariage", message)
//        goToCurrentWedding(weddingCreated.uid).flashing("success" -> Messages("main.wedding.weddingCreated"))
        Redirect(routes.Wedding.display(weddingCreated.uid).url, Map(helpers.TutorialHelper.forceTutorialParameter -> Seq(helpers.TutorialHelper.forceTutorialParameter))).flashing("success" -> Messages("main.wedding.weddingCreated"))
      }
    )
  }

  /**
   * Display a wedding edition form
   * @param uid
   */
  def edit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => Ok(views.html.wedding.edit(uid, weddingForm.fill(wedding), weddingUidForm.fill(uid, uid), weddingCodeForm.fill(wedding.code), wedding))
      }.getOrElse(NotFound)
  }

  /**
   * Handle wedding edition form submission
   * @param uid
   */
  def doEdit(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          weddingForm.bindFromRequest.fold(
            formWithErrors => BadRequest(views.html.wedding.edit(uid, formWithErrors, weddingUidForm.fill(uid, uid), weddingCodeForm.fill(wedding.code), wedding)),
            weddingForm => {
              models.wedding.Wedding.edit(weddingForm.copy(id = wedding.id))
              goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.wedding.modificationsSaved"))
            }
          )
      }.getOrElse(NotFound)
  }

  /**
   * Handle wedding code edition form submission
   * @param uid
   */
  def doEditCode(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          weddingCodeForm.bindFromRequest.fold(
            formWithErrors => BadRequest(views.html.wedding.edit(uid, weddingForm.fill(wedding), weddingUidForm.fill(uid, uid), formWithErrors, wedding)),
            weddingCode => {
              models.wedding.Wedding.editCode(wedding.copy(code = weddingCode))
              goToCurrentWeddingEdit(uid).flashing("success" -> Messages("main.wedding.modificationsSaved"))
            }
          )
      }.getOrElse(NotFound)
  }


  /**
   * Handle wedding change uid form submission
   * @param uid
   */
  def doChangeUid(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          weddingUidForm.bindFromRequest.fold(
            formWithErrors => BadRequest(views.html.wedding.edit(uid, weddingForm.fill(wedding), formWithErrors, weddingCodeForm.fill(wedding.code), wedding)),
            weddingUidForm => {
              models.wedding.Wedding.edit(wedding.copy(uid = weddingUidForm._2))
              goToCurrentWeddingEdit(weddingUidForm._2).flashing("success" -> Messages("main.wedding.modificationsSaved"))
            }
          )
      }.getOrElse(NotFound)
  }

  /**
   * Handle wedding deletion
   * @param uid
   */
  def doDelete(uid: String) = SecuredAction(IsAdministrator()) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => {
          models.wedding.Wedding.delete(wedding)
          Home.flashing("success" -> Messages("main.wedding.weddingDeleted"))
        }
      }.getOrElse(NotFound)
  }

  /**
   * Send a PDF file with main info
   * @param uid the wedding UID
   */
  def print(uid: String) = UserAwareAction {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding =>
          Ok(views.html.wedding.print(wedding))
          //Ok(util.pdf.PDF.toBytes(views.html.wedding.print.render(wedding, flash, request, user, lang))).as("application/pdf").withHeaders(("Content-Disposition", "attachment; filename=\"info.pdf\""))
      }.getOrElse(NotFound)
  }


  /**
   * Display the sharing page
   * @param uid the wedding UID
   */
  def share(uid: String) = SecuredAction(IsOwnerOfWedding(uid)) {
    implicit request =>
      models.wedding.Wedding.findByUid(uid).map {
        wedding => Ok(views.html.wedding.share(wedding))
      }.getOrElse(NotFound)
  }

  /**
   * Redirect to the current wedding display
   */
  def goToCurrentWedding(uid: String) = Redirect(routes.Wedding.display(uid))

  /**
   * Redirect to the edit form of the current wedding
   */
  def goToCurrentWeddingEdit(uid: String) = Redirect(routes.Wedding.display(uid))
//  def goToCurrentWeddingEdit(uid: String) = Redirect(routes.Wedding.edit(uid))
}