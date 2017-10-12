package controllers.helpers

import views.html.helper.FieldConstructor

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.02.13
 * Time: 15:15
 * To change this template use File | Settings | File Templates.
 */
object BootstrapHelper {
  implicit val myFields = FieldConstructor(views.html.helper.bootstrapInput.f)
}
