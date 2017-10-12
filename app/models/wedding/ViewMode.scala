package models.wedding

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 06.03.14
 * Time: 14:29
 * To change this template use File | Settings | File Templates.
 */
sealed trait ViewMode
object ViewMode {
  case object GuestViewMode extends ViewMode
  case object OwnerViewMode extends ViewMode
}