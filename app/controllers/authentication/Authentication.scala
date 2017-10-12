package controllers.authentication

import controllers.AnyController
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 05.10.12
 * Time: 15:43
 * To change this template use File | Settings | File Templates.
 */
object Authentication extends AnyController  {
  /**
   * Profile form definition
   */
  val profileForm = Form(
    tuple (
      "firstName" -> nonEmptyText(maxLength = 49),
      "lastName" -> nonEmptyText(maxLength = 50)
    )
  )

  /**
   * Display edit profile form
   */
  def edit = SecuredAction(IsMethodUserPassword()) {
    implicit request => {
      val profileUser = request.user.asInstanceOf[models.authentication.User]
      Ok(views.html.authentication.profile(profileForm.fill(profileUser.firstName, profileUser.lastName)))
    }
  }

  /**
   * Handle edit profile form submission
   */
  def doEdit = SecuredAction(IsMethodUserPassword()) {
    implicit request =>
      profileForm.bindFromRequest.fold(
        formWithErrors =>
          BadRequest(views.html.authentication.profile(formWithErrors)),
        profile => {
          val user = request.user.asInstanceOf[models.authentication.User]
          val fName = profile._1
          val lName = profile._2
          val flName = (fName + " " + lName).trim
          models.authentication.User.updateUser(user.copy(firstName = fName, lastName = lName, fullName = flName))
          Redirect(controllers.routes.Wedding.index).flashing("success" -> Messages("main.authentication.profileUpdateSuccessful"))
        }
      )
  }
}
