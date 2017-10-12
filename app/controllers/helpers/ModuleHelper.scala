package controllers.helpers

import play.api.i18n.{Lang, Messages}
import models.authentication.User
import collection.immutable.{TreeMap, HashMap}
import controllers.routes

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.10.12
 * Time: 13:14
 * To change this template use File | Settings | File Templates.
 */
object ModuleHelper {

  /**
   * Return the instructions label to be displayed above the module
   * @param wedding the wedding
   * @param moduleId the module ID
   */
  def getInstructions(wedding: models.wedding.Wedding, moduleId: Int)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): String = moduleId match {
    case models.modules.Contact.ID => {
      wedding.modules.find(m => m.getId.get == moduleId) match {
        case Some(contact) =>
          contact.getInstructions.getOrElse(Messages("main.modules.contact.explanation"))
        case None => ""
      }
    }
    case models.modules.Register.ID => {
      wedding.modules.find(m => m.getId.get == moduleId) match {
        case Some(register) =>
          register.getInstructions.getOrElse(Messages("main.modules.register.explanation"))
        case None => ""
      }
    }
    case models.modules.GiftList.ID => {
      wedding.modules.find(m => m.getId.get == moduleId) match {
        case Some(giftList) =>
          giftList.getInstructions.getOrElse(Messages("main.modules.giftList.explanation"))
        case None => ""
      }
    }
    case _ => ""
  }


    /**
   * Return the edit link for a particular module of a wedding if available
   * @param wedding the wedding
   * @param moduleId the module ID
   */
  def getEditLink(wedding: models.wedding.Wedding, moduleId: Int)(implicit lang: Lang) = moduleId match {
    case models.modules.MainModule.ID => Some(routes.Wedding.edit(wedding.uid))
    case models.modules.WelcomePic.ID => Some(controllers.modules.welcomePic.routes.WelcomePic.editWelcomePic(wedding.uid))
    case models.modules.RichText.ID => Some(controllers.modules.routes.RichText.edit(wedding.uid))
    case models.modules.GeoLocal.ID => Some(controllers.modules.routes.GeoLocal.edit(wedding.uid))
    case models.modules.Gallery.ID => Some(controllers.modules.gallery.routes.Gallery.editGallery(wedding.uid))
    case models.modules.Customize.ID => Some(controllers.modules.customize.routes.Customize.edit(wedding.uid))
    case models.modules.Register.ID => Some(controllers.modules.routes.Register.edit(wedding.uid))
    case models.modules.GiftList.ID => Some(controllers.modules.routes.GiftList.edit(wedding.uid))
    case models.modules.Budget.ID => Some(controllers.modules.routes.Budget.edit(wedding.uid))
    case models.modules.Contact.ID => Some(controllers.modules.routes.Contact.edit(wedding.uid))
    case _ => None
  }

  /**
   * Return the link to display the module info/details
   * @param wedding the wedding
   * @param moduleId the module ID
   */
  def getDisplayLink(wedding: models.wedding.Wedding, moduleId: Int)(implicit lang: Lang) = moduleId match {
    case models.modules.GeoLocal.ID => Some(controllers.modules.routes.GeoLocal.display(wedding.uid))
    case models.modules.Gallery.ID => Some(controllers.modules.gallery.routes.Gallery.display(wedding.uid))
    case models.modules.Register.ID => Some(controllers.modules.routes.Register.register(wedding.uid))
    case models.modules.GiftList.ID => Some(controllers.modules.routes.GiftList.display(wedding.uid))
    case models.modules.Budget.ID => Some(controllers.modules.routes.Budget.edit(wedding.uid))
    case models.modules.Contact.ID => Some(controllers.modules.routes.Contact.display(wedding.uid))
    case _ => None
  }

  /**
   * Check if a given module exists
   */
  def isModuleDefined(wedding: models.wedding.Wedding, moduleId: Long)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    wedding.modules.exists(module => module.getId.get == moduleId)
  }

  /**
   * Check if a module can be displayed or not
   */
  def isModuleReadyForDisplay(wedding: models.wedding.Wedding, moduleId: Long)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang): Boolean = moduleId match {
    case models.modules.Contact.ID => isContact(wedding)
    case models.modules.GiftList.ID => isGiftList(wedding)
    case _ => true
  }

  /**
   * Check if the module Rich Text is available
   */
  def isRichText(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.RichText.ID)
  }

  /**
   * Check if the module Contact is available
   */
  def isContact(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang) = {
    if (isModuleDefined(wedding, models.modules.Contact.ID)) {
      if (AuthenticationHelper.isOwnerOfWedding(wedding.uid)) {
        true
      } else {
        isContactReady(wedding)
      }
    } else {
      false
    }
  }

  /**
   * Check if the contact can be used (is not empty)
   */
  def isContactReady(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang): Boolean = {
    wedding.modules.find(m => m.getId.get.toInt == models.modules.Contact.ID) match {
      case Some(contact: models.modules.Contact) => {
        val contactInfo = contact.moduleContent
        contactInfo.coupleAddressName.isDefined ||
        contactInfo.coupleAddressPlace.isDefined ||
        contactInfo.coupleAddressStreet.isDefined ||
        contactInfo.coupleAddressZip.isDefined ||
        contactInfo.coupleMail.isDefined ||
        contactInfo.couplePhone.isDefined ||
        contactInfo.organizerMail.isDefined ||
        contactInfo.organizerPhone.isDefined
      }
      case _ => false
    }
  }

  /**
   * Display the HTML template with the contact infos
   */
  def getContact(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    controllers.modules.Contact.getContact(wedding)
  }

  /**
   * Display the summarized HTML template with the basic contact infos
   */
  def getContactSummary(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    controllers.modules.Contact.getContactSummary(wedding)
  }

  /**
   * Return a sequence of the authorized mail recipients, which can be used as the possible options of a select input
   */
  def getOptionsAuthorizedRecipients(contact: models.modules.Contact)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    var optionsRecipient = Seq[(String, String)]()
    if (UtilsHelper.isNotEmpty(contact.moduleContent.coupleMail)) {
      optionsRecipient = optionsRecipient :+ models.modules.Contact.RECIPIENT_COUPLE.toString -> Messages("main.modules.contact.couple")
    }
    if (UtilsHelper.isNotEmpty(contact.moduleContent.organizerMail)) {
      optionsRecipient = optionsRecipient :+ models.modules.Contact.RECIPIENT_ORGANIZER.toString -> Messages("main.modules.contact.organizer")
    }
    optionsRecipient
  }

  /**
   * Check if the module Register is available
   */
  def isRegister(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.Register.ID)
  }

  /**
   * Display the module AdFree banner if available
   */
  def getAdFreeBanner(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang, isOwnerOfWedding: Boolean, viewMode: models.wedding.ViewMode) = {
    if (!isModuleDefined(wedding, models.modules.AdFree.ID)) {
      views.html.modules.adFree.banner(wedding)
    }
  }

  /**
   * Display the module AdFree portlet if available
   */
  def getAdFreePortlet(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang, isOwnerOfWedding: Boolean, viewMode: models.wedding.ViewMode) = {
    if (!isModuleDefined(wedding, models.modules.AdFree.ID)) {
      views.html.modules.adFree.portlet(wedding)
    }
  }

  /**
   * Check if the module GeoLocal is available
   */
  def isGeoLocal(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.GeoLocal.ID)
  }

  /**
   * Retrieve the geo localization data (markers)
   */
  def getGeoLocal(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    wedding.modules.find(m => m.getId.get.toInt == models.modules.GeoLocal.ID) match {
      case Some(geoLocal: models.modules.GeoLocal) => Some(geoLocal)
      case _ => None
    }
  }

  /**
   * Retrieve the marker icons list
   */
  def getGeoLocalMarkerIcons = {
    TreeMap[Int, String](
      (0, "images/markers/wedding.png"),
      (1, "images/markers/church.png"),
      (2, "images/markers/townhall.png"),
      (3, "images/markers/restaurant.png"),
      (4, "images/markers/coktail.png"),
      (5, "images/markers/fastfood.png"),
      (6, "images/markers/hotel.png"),
      (7, "images/markers/palace.png"),
      (8, "images/markers/parking.png"),
      (9, "images/markers/train.png"),
      (10, "images/markers/toilets.png"),
      (11, "images/markers/here.png")
    )
  }

  /**
   * Check if the module GiftList is available
   */
  def isGiftList(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang) = {
    if (isModuleDefined(wedding, models.modules.GiftList.ID)) {
      if (AuthenticationHelper.isOwnerOfWedding(wedding.uid)) {
        true
      } else {
        isGiftListReady(wedding)
      }
    } else {
      false
    }
  }

  /**
   * Check if the gift list can be used (is correctly configured)
   */
  def isGiftListReady(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang): Boolean = {
    wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
      case Some(giftList: models.modules.GiftList) => giftList.moduleContent.paypalInfo.status == models.modules.GiftList.PAYPAL_PERMISSIONS_STATUS_VERIFIED
      case _ => false
    }
  }

  /**
   * Check if the module Budget is available
   */
  def isBudget(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.Budget.ID)
  }

  /**
   * Check if the module Gallery is available
   */
  def isGallery(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.Gallery.ID)
  }

  /**
   * Retrieve the gallery (and display albums given their visibility)
   */
  def getGallery(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang) = {
    models.modules.Gallery.loadGallery(wedding.id) match {
      case Some(gallery: models.modules.Gallery) => {
        val isOwner = AuthenticationHelper.isOwnerOfWedding(wedding.uid)
        val isGuestsWithRestrictedArea = AuthenticationHelper.isGuestWithRestrictedArea(wedding.uid)
        val allowedVisibility = isOwner match {
          case true => models.modules.Gallery.VISIBILITY_OWNER
          case false => isGuestsWithRestrictedArea match {
            case true => models.modules.Gallery.VISIBILITY_VIP
            case false => models.modules.Gallery.VISIBILITY_GUESTS
          }
        }
        val galleryInfo = gallery.moduleContent.copy(albums = gallery.moduleContent.albums.filter(a => a.visibility <= allowedVisibility))
        Some(gallery.copy(moduleContent = galleryInfo))
      }
      case _ => None
    }
  }

  /**
   * Return the possibile visibility values with their label
   */
  def getGalleryVisibilityValues(implicit  lang: Lang) =
    Map(models.modules.Gallery.VISIBILITY_GUESTS -> Messages("main.modules.gallery.visibilityGuests"), models.modules.Gallery.VISIBILITY_VIP -> Messages("main.modules.gallery.visibilityVIP"), models.modules.Gallery.VISIBILITY_OWNER -> Messages("main.modules.gallery.visibilityOwner"))

  /**
   * Check if the module Welcome Pic is available
   */
  def isWelcomePic(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.WelcomePic.ID)
  }

  /**
   * Display the HTML template with the Welcome Picture of the wedding
   */
  def getWelcomePic(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    wedding.modules.find(module => module.getId.get == models.modules.WelcomePic.ID) match {
      case Some(module) =>
        module match {
          case welcomePic: models.modules.WelcomePic =>
            views.html.modules.welcomePic.welcomePicture(wedding, welcomePic.moduleContent)
        }
      case _ => views.html.modules.notFound(wedding)
    }
  }

  /**
   * Check if the module Customization module is available
   */
  def isCustomization(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    isModuleDefined(wedding, models.modules.Customize.ID)
  }

  /**
   * Display the HTML template with the Customization info of the wedding to be inserted in the head tag
   */
  def getCustomizationHead(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    wedding.modules.find(module => module.getId.get == models.modules.Customize.ID) match {
      case Some(module) =>
        module match {
          case customize: models.modules.Customize =>
            customize.moduleContent match {
              case customizeData: models.modules.CustomizeData =>
                views.html.modules.customize.displayHead(customize)
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
  }

  /**
   * Display the HTML template with the Customization info of the wedding to be inserted in the head tag
   */
  def getCustomizationBody(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    wedding.modules.find(module => module.getId.get == models.modules.Customize.ID) match {
      case Some(module) =>
        module match {
          case customize: models.modules.Customize =>
            customize.moduleContent match {
              case customizeData: models.modules.CustomizeData =>
                views.html.modules.customize.displayBody(customize)
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
  }

  /**
   * Return a sequence of the available customization fonts, which can be used as the possible options of a select input
   */
  def getOptionsCustomizeFont(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    for (font <- models.modules.Customize.loadAvailableFonts) yield (font.id.get.toString, font.name + ((font.id.get.toInt == models.modules.Customize.DEFAULT_FONT_ID) match { case true => {" ("+Messages("main.modules.customize.default")+")"} case false => {""} } ) )
  }

  /**
   * Return the list of the available customization fonts
   */
  def getCustomizeFonts(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    models.modules.Customize.loadAvailableFonts
  }

  /**
   * Return a sequence of the available customization themes, which can be used as the possible options of a select input
   */
  def getOptionsCustomizeTheme(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    for (theme <- models.modules.Customize.loadAvailableThemes.sortBy(_.name))
      yield (theme.id.get.toString, theme.name + (theme.id.get.toInt match {
        case models.modules.Customize.DEFAULT_THEME_ID => {" ("+Messages("main.modules.customize.default")+")"}
        case models.modules.Customize.CUSTOM_THEME_ID => {" ("+Messages("main.modules.customize.makeItYourself")+")"}
        case _ => {""}
    } ) )
  }

  /**
   * Return a sequence of the available customization themes
   */
  def getCustomizeThemePreviewPath(implicit request: play.api.mvc.RequestHeader, lang: Lang) = {
    for (theme <- models.modules.Customize.loadAvailableThemes if theme.id.get != models.modules.Customize.CUSTOM_THEME_ID)
      yield (theme.id.get.toString, theme.name, theme.path)
  }

  /**
   * Return the sorted list of modules for the given column
   */
  def getOrderedModulesForColumn(column: Int, wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang) = {
    val sorted = sortModules(wedding.modules.filter(module => module.getDisplayColumn.isDefined && module.getDisplayColumn.get == column))
    sorted.filter(module => module.isAuthorized(wedding) && ModuleHelper.isModuleReadyForDisplay(wedding, module.getId.get))
  }

  /**
   * Sort a modules list
   */
  def sortModules(modules: List[models.modules.Module])(implicit request: play.api.mvc.RequestHeader, lang: Lang): List[models.modules.Module] = {
    if (modules.isEmpty) {
      modules
    } else {
      List() ::: sortModulesWithPrevious(modules, None)
    }
  }

  /**
   * Sort a modules list given the last sorted module (previous module)
   */
  protected def sortModulesWithPrevious(modules: List[models.modules.Module], previousModule: Option[models.modules.Module]): List[models.modules.Module] = {
    if (modules.isEmpty) {
      Nil
    } else {
      val nextModule: Option[models.modules.Module] = previousModule match {
        case Some(prev) => modules.find(module => module.getDisplayPreviousModuleId.isDefined && module.getDisplayPreviousModuleId.get == prev.getId.get) match {
          case Some(next) => Some(next)
          case None => modules.find(_.getDisplayPreviousModuleId.isEmpty)
        }
        case None => modules.find(_.getDisplayPreviousModuleId.isEmpty)
      }
      nextModule match {
        case Some(module) => {
          module :: sortModulesWithPrevious(modules.filter(_.getId.get != module.getId.get), Some(module))
        }
        case None => {
          modules.head :: sortModulesWithPrevious(modules.tail, Some(modules.head))
        }
      }
    }
  }

  /**
   * Return a JS-style array of available emails addresses for the given wedding
   * @param wedding the wedding
   */
  def getAvailableEmailAddresses(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, user: Option[models.authentication.User], lang: Lang): String = {
    val addresses: List[String] =
      if (user.isDefined && user.get.email.isDefined) {
        user.get.email.get :: Nil
      } else {
        List()
      }
    "["+addresses.map(a => "\""+a+"\"").mkString(",")+"]"
  }
}