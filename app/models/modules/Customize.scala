package models.modules

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import anorm.~
import anorm.Id
import play.api.Play.current
import java.sql.Connection
import play.api.i18n.Lang
import java.io.File
import controllers.modules.customize.CustomizePicturePlugin
import com.typesafe.plugin._

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 27.02.13
 * Time: 09:40
 * To change this template use File | Settings | File Templates.
 */
case class Customize(weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: CustomizeInfo, moduleActive: Boolean, movieUrl: Option[String])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SYSTEM, None, None) {
  override def init()(implicit connection: Connection) = {
    Customize.addCustomization(this)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    Customize.deleteModuleForWedding(weddingId, this)
  }

  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] = None
  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    views.html.modules.empty(wedding)
  }
}

abstract class CustomizeInfo(customizedFont: CustomizeFont, customizedTheme: CustomizeTheme) {
  def font = customizedFont
  def theme = customizedTheme
}
case class CustomizeData(customizedFont: CustomizeFont, customizedTheme: CustomizeTheme) extends CustomizeInfo(customizedFont, customizedTheme)
case class DefaultData() extends CustomizeInfo(Customize.loadDefaultFont, Customize.loadDefaultTheme)

case class CustomizeFont(id: Pk[Long], name: String, parameterValue: String, size: Double)
case class CustomizeTheme(id: Pk[Long], name: String, path: String, hasPicture: Boolean, colors: Option[CustomizeThemeColors]) {
  def hasBigPicture: Boolean = {
    hasPicture || (
      id.get == models.modules.Customize.CUSTOM_THEME_ID
        && colors.isDefined
        && colors.get.backgroundAttributes.backgroundPic.isDefined
        && colors.get.backgroundAttributes.backgroundPicPos.isDefined
        && colors.get.backgroundAttributes.backgroundPicPos.get == models.modules.Customize.CUSTOM_PICTURE_POSITION_STRETCH
    )
  }
}

case class CustomizeThemeColors(generalAttributes: CustomizeThemeAttributesGeneral, menuAttributes: CustomizeThemeAttributesMenu,
                                backgroundAttributes: CustomizeThemeAttributesBackground, centralAttributes: CustomizeThemeAttributesCentral, mainAttributes: CustomizeThemeAttributesMain)

case class CustomizeThemeAttributesGeneral(generalPrimaryBtnBackColor: Option[String] = None, generalPrimaryBtnFontColor: Option[String] = None, generalBtnFontColor: Option[String] = None, generalLinkFontColor: Option[String] = None, generalTextFontColor: Option[String] = None)
case class CustomizeThemeAttributesMenu(menuFontColor: Option[String] = None, menuBackColor: Option[String] = None, menuBackTransparent: Option[Boolean] = None, menuCornerRadius: Option[Int] = None)
case class CustomizeThemeAttributesBackground(backgroundPic: Option[CustomizePicture] = None, backgroundPicPos: Option[Int] = None, backgroundBackColor: Option[String] = None)
case class CustomizeThemeAttributesCentral(centralBackPic: Option[CustomizePicture] = None, centralBackPicPos: Option[Int] = None, centralBackColorTop: Option[String] = None, centralBackColorBottom: Option[String] = None, centralBackTransparent: Option[Boolean] = None, centralShadowH: Option[Int] = None, centralShadowV: Option[Int] = None, centralShadowBlur: Option[Int] = None, centralShadowSpread: Option[Int] = None, centralShadowColor: Option[String] = None, centralCornerRadius: Option[Int] = None)
case class CustomizeThemeAttributesMain(mainBackColor: Option[String] = None, mainBackTransparent: Option[Boolean] = None, mainCornerRadius: Option[Int] = None, mainSeparatorVisible: Option[Boolean] = None, mainSeparatorColor: Option[String] = None)

case class CustomizeThemePicture(filename: Option[String], contentType: Option[String])

abstract class CustomizePicture(weddingId: Pk[Long], filename: Option[String], contentType: Option[String]) {
  def picType: Int
  def key: String
  def getContentType = contentType
  def getFilename = filename
  def getWeddingId = weddingId
}
case class CustomizePictureBackground(weddingId: Pk[Long], filename: Option[String], contentType: Option[String]) extends CustomizePicture(weddingId, filename, contentType) {
  def picType: Int = models.modules.Customize.PIC_TYPE_BACKGROUND
  def key: String = {
    weddingId.get.toString+"/background"
  }
}
case class CustomizePictureCentral(weddingId: Pk[Long], filename: Option[String], contentType: Option[String]) extends CustomizePicture(weddingId, filename, contentType) {
  def picType: Int = models.modules.Customize.PIC_TYPE_CENTRAL
  def key: String = {
    weddingId.get.toString+"/central"
  }
}
case class CustomizePictureLogo(weddingId: Pk[Long], filename: Option[String], contentType: Option[String]) extends CustomizePicture(weddingId, filename, contentType) {
  def picType: Int = models.modules.Customize.PIC_TYPE_LOGO
  def key: String = {
    weddingId.get.toString+"/logo"
  }
}

object Customize {
  /**
   * Images store definition
   */
  val store = use[CustomizePicturePlugin]

  val ID = 10
  val NAME = "Customize"

  val PIC_TYPE_BACKGROUND = 0
  val PIC_TYPE_CENTRAL = 1
  val PIC_TYPE_LOGO = 2

  val DEFAULT_FONT_ID = 0
  val DEFAULT_THEME_ID = 0

  val CUSTOM_THEME_ID = 6
  val CUSTOM_MAIN_COLOR = "1C7BA0"
  val CUSTOM_SECOND_COLOR = "FFFFFF"
  val CUSTOM_GRADIENT_START_COLOR = "FFFFFF"
  val CUSTOM_GRADIENT_END_COLOR = "1C7BA0"

  val CUSTOM_DEFAULT_COLORS = CustomizeThemeColors(CustomizeThemeAttributesGeneral(), CustomizeThemeAttributesMenu(), CustomizeThemeAttributesBackground(), CustomizeThemeAttributesCentral(), CustomizeThemeAttributesMain())

  val CUSTOM_PICTURE_POSITION_STRETCH = 0
  val CUSTOM_PICTURE_POSITION_CENTERED = 1
  val CUSTOM_PICTURE_POSITION_MOSAIC = 2
  val CUSTOM_PICTURE_POSITION = List(CUSTOM_PICTURE_POSITION_STRETCH, CUSTOM_PICTURE_POSITION_CENTERED, CUSTOM_PICTURE_POSITION_MOSAIC)

  val BACKGROUND_PICTURE_MAX_SIZE = 250
  val CENTRAL_PICTURE_MAX_SIZE = 250

  /**
   * Parse customization info
   */
  val customize = {
    get[Pk[Long]]("weddingid") ~
      get[Long]("fontid") ~
      get[Long]("themeid") ~
      get[Boolean]("active") map {
      case weddingId ~ fontId ~ themeId ~ active
        => Customize(weddingId, Id(Customize.ID), Customize.NAME, CustomizeData(loadFont(Id(fontId)), loadTheme(Id(themeId), weddingId)), active, None)
    }
  }

  /**
   * Parse font info
   */
  val font = {
    get[Pk[Long]]("id") ~
      get[String]("name") ~
      get[String]("parametervalue") ~
      get[Double]("size") map {
      case fontId ~ name ~ parameterValue ~ size
        => CustomizeFont(fontId, name, parameterValue, size)
    }
  }

  /**
   * Parse theme info
   */
  val theme = {
    get[Pk[Long]]("id") ~
      get[String]("name") ~
      get[String]("path") ~
      get[Boolean]("haspicture") map {
      case themeId ~ name ~ path ~ hasPicture
        => CustomizeTheme(themeId, name, path, hasPicture, None)
    }
  }

  /**
   * Parse colors info
   */
  val colors = {
    get[Pk[Long]]("weddingid") ~
      get[Option[String]]("generalprimarybtnbackcolor") ~
      get[Option[String]]("generalprimarybtnfontcolor") ~
      get[Option[String]]("generalbtnfontcolor") ~
      get[Option[String]]("generallinkfontcolor") ~
      get[Option[String]]("generaltextfontcolor") ~
      get[Option[String]]("menufontcolor") ~
      get[Option[String]]("menubackcolor") ~
      get[Option[Boolean]]("menubacktransparent") ~
      get[Option[Int]]("menucornerradius") ~
      get[Option[String]]("backgroundpicfilename") ~
      get[Option[String]]("backgroundpiccontenttype") ~
      get[Option[Int]]("backgroundpicpos") ~
      get[Option[String]]("backgroundbackcolor") ~
      get[Option[String]]("centralbackpicfilename") ~
      get[Option[String]]("centralbackpiccontenttype") ~
      get[Option[Int]]("centralbackpicpos") ~
      get[Option[String]]("centralbackcolortop") ~
      get[Option[String]]("centralbackcolorbottom") ~
      get[Option[Boolean]]("centralbacktransparent") ~
      get[Option[Int]]("centralshadowh") ~
      get[Option[Int]]("centralshadowv") ~
      get[Option[Int]]("centralshadowblur") ~
      get[Option[Int]]("centralshadowspread") ~
      get[Option[String]]("centralshadowcolor") ~
      get[Option[Int]]("centralcornerradius") ~
      get[Option[String]]("mainbackcolor") ~
      get[Option[Boolean]]("mainbacktransparent") ~
      get[Option[Int]]("maincornerradius") ~
      get[Option[Boolean]]("mainsepvisible") ~
      get[Option[String]]("mainsepcolor") map {
      case weddingId ~
        generalprimarybtnbackcolor ~ generalprimarybtnfontcolor ~ generalbtnfontcolor ~ generallinkfontcolor ~ generaltextfontcolor ~
        menufontcolor ~ menubackcolor ~ menubacktransparent ~ menucornerradius ~
        backgroundpicfilename ~ backgroundpiccontenttype ~ backgroundpicpos ~ backgroundbackcolor ~
        centralbackpicfilename ~ centralbackpiccontenttype ~ centralbackpicpos ~ centralbackcolortop ~ centralbackcolorbottom ~ centralbacktransparent ~ centralshadowh ~ centralshadowv ~ centralshadowblur ~ centralshadowspread ~ centralshadowcolor ~ centralcornerradius ~
        mainbackcolor ~ mainbacktransparent ~ maincornerradius ~ mainsepvisible ~ mainsepcolor
      => CustomizeThemeColors(
          getGeneralAttributes(generalprimarybtnbackcolor, generalprimarybtnfontcolor, generalbtnfontcolor, generallinkfontcolor, generaltextfontcolor),
          getMenuAttributes(menufontcolor, menubackcolor, menubacktransparent, menucornerradius),
          getBackgroundAttributes(weddingId, backgroundpicfilename, backgroundpiccontenttype, backgroundpicpos, backgroundbackcolor),
          getCentralAttributes(weddingId, centralbackpicfilename, centralbackpiccontenttype, centralbackpicpos, centralbackcolortop, centralbackcolorbottom, centralbacktransparent, centralshadowh, centralshadowv, centralshadowblur, centralshadowspread, centralshadowcolor, centralcornerradius),
          getMainAttributes(mainbackcolor, mainbacktransparent, maincornerradius, mainsepvisible, mainsepcolor)
        )
    }
  }

  def getGeneralAttributes(generalPrimaryBtnBackColor: Option[String], generalPrimaryBtnFontColor: Option[String], generalBtnFontColor: Option[String], generalLinkFontColor: Option[String], generalTextFontColor: Option[String]): CustomizeThemeAttributesGeneral = {
    CustomizeThemeAttributesGeneral(generalPrimaryBtnBackColor, generalPrimaryBtnFontColor, generalBtnFontColor, generalLinkFontColor, generalTextFontColor)
  }

  def getMenuAttributes(menuFontColor: Option[String], menuBackColor: Option[String], menuBackTransparent: Option[Boolean], menuCornerRadius: Option[Int]): CustomizeThemeAttributesMenu = {
    CustomizeThemeAttributesMenu(menuFontColor, menuBackColor, menuBackTransparent, menuCornerRadius)
  }

  def getBackgroundAttributes(weddingId: Pk[Long], backgroundPicFilename: Option[String], backgroundPicContentType: Option[String], backgroundPicPos: Option[Int], backgroundBackColor: Option[String]) = {
    val pic = backgroundPicFilename match {
      case Some(filename) => Some(CustomizePictureBackground(weddingId, Some(filename), backgroundPicContentType))
      case None => None
    }
    CustomizeThemeAttributesBackground(pic, backgroundPicPos, backgroundBackColor)
  }

  def getCentralAttributes(weddingId: Pk[Long], centralBackPicFilename: Option[String], centralBackPicContentType: Option[String], centralBackPicPos: Option[Int], centralBackColorTop: Option[String], centralBackColorBottom: Option[String], centralBackTransparent: Option[Boolean], centralShadowH: Option[Int], centralShadowV: Option[Int], centralShadowBlur: Option[Int], centralShadowSpread: Option[Int], centralShadowColor: Option[String], centralCornerRadius: Option[Int]) = {
    val pic = centralBackPicFilename match {
      case Some(filename) => Some(CustomizePictureCentral(weddingId, Some(filename), centralBackPicContentType))
      case None => None
    }
    CustomizeThemeAttributesCentral(pic, centralBackPicPos, centralBackColorTop, centralBackColorBottom, centralBackTransparent, centralShadowH, centralShadowV, centralShadowBlur, centralShadowSpread, centralShadowColor, centralCornerRadius)
  }

  def getMainAttributes(mainBackColor: Option[String], mainBackTransparent: Option[Boolean], mainCornerRadius: Option[Int], mainSeparatorVisible: Option[Boolean], mainSeparatorColor: Option[String]): CustomizeThemeAttributesMain = {
    CustomizeThemeAttributesMain(mainBackColor, mainBackTransparent, mainCornerRadius, mainSeparatorVisible, mainSeparatorColor)
  }

  /**
   * Insert a customization for the given wedding
   * @param customize the initial value for the customization
   */
  def addCustomization(customize: Customize)(implicit connection: Connection) = {
    SQL(
      """
        INSERT INTO mod_customize (
          weddingid, fontid, themeid
        ) VALUES (
          {weddingid}, {fontid}, {themeid}
        )
      """
    ).on(
      'weddingid -> customize.weddingId,
      'fontid -> customize.moduleContent.font.id,
      'themeid -> customize.moduleContent.theme.id
    ).executeUpdate()

    SQL(
      """
        INSERT INTO mod_customize_colors (
          weddingid
        ) VALUES (
          {weddingid}
        )
      """
    ).on(
      'weddingid -> customize.weddingId
    ).executeUpdate()
  }

  /**
   * Update the customization info for a given wedding
   * @param customize the value for the customization
   */
  def editCustomization(customize: Customize) = {
    DB.withTransaction {
      implicit connection =>
        SQL(
          """
            UPDATE mod_customize SET
              fontid = {fontid}, themeid = {themeid}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> customize.weddingId,
          'fontid -> customize.moduleContent.font.id,
          'themeid -> customize.moduleContent.theme.id
        ).executeUpdate()

      if (customize.moduleContent.theme.id.get.toInt == CUSTOM_THEME_ID && customize.moduleContent.theme.colors.isDefined) {
        SQL(
          """
            UPDATE mod_customize_colors SET
              generalprimarybtnbackcolor = {generalprimarybtnbackcolor},
              generalprimarybtnfontcolor = {generalprimarybtnfontcolor},
              generalbtnfontcolor = {generalbtnfontcolor},
              generallinkfontcolor = {generallinkfontcolor},
              generaltextfontcolor = {generaltextfontcolor},
              menufontcolor = {menufontcolor},
              menubackcolor = {menubackcolor},
              menubacktransparent = {menubacktransparent},
              menucornerradius = {menucornerradius},
              backgroundpicpos = {backgroundpicpos},
              backgroundbackcolor = {backgroundbackcolor},
              centralbackpicpos = {centralbackpicpos},
              centralbackcolortop = {centralbackcolortop},
              centralbackcolorbottom = {centralbackcolorbottom},
              centralbacktransparent = {centralbacktransparent},
              centralshadowh = {centralshadowh},
              centralshadowv = {centralshadowv},
              centralshadowblur = {centralshadowblur},
              centralshadowspread = {centralshadowspread},
              centralshadowcolor = {centralshadowcolor},
              centralcornerradius = {centralcornerradius},
              mainbackcolor = {mainbackcolor},
              mainbacktransparent = {mainbacktransparent},
              maincornerradius = {maincornerradius},
              mainsepvisible = {mainsepvisible},
              mainsepcolor = {mainsepcolor}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> customize.weddingId,
          'generalprimarybtnbackcolor -> customize.moduleContent.theme.colors.get.generalAttributes.generalPrimaryBtnBackColor,
          'generalprimarybtnfontcolor -> customize.moduleContent.theme.colors.get.generalAttributes.generalPrimaryBtnFontColor,
          'generalbtnfontcolor -> customize.moduleContent.theme.colors.get.generalAttributes.generalBtnFontColor,
          'generallinkfontcolor -> customize.moduleContent.theme.colors.get.generalAttributes.generalLinkFontColor,
          'generaltextfontcolor -> customize.moduleContent.theme.colors.get.generalAttributes.generalTextFontColor,
          'menufontcolor -> customize.moduleContent.theme.colors.get.menuAttributes.menuFontColor,
          'menubackcolor -> customize.moduleContent.theme.colors.get.menuAttributes.menuBackColor,
          'menubacktransparent -> customize.moduleContent.theme.colors.get.menuAttributes.menuBackTransparent,
          'menucornerradius -> customize.moduleContent.theme.colors.get.menuAttributes.menuCornerRadius,
          'backgroundpicpos -> customize.moduleContent.theme.colors.get.backgroundAttributes.backgroundPicPos,
          'backgroundbackcolor -> customize.moduleContent.theme.colors.get.backgroundAttributes.backgroundBackColor,
          'centralbackpicpos -> customize.moduleContent.theme.colors.get.centralAttributes.centralBackPicPos,
          'centralbackcolortop -> customize.moduleContent.theme.colors.get.centralAttributes.centralBackColorTop,
          'centralbackcolorbottom -> customize.moduleContent.theme.colors.get.centralAttributes.centralBackColorBottom,
          'centralbacktransparent -> customize.moduleContent.theme.colors.get.centralAttributes.centralBackTransparent,
          'centralshadowh -> customize.moduleContent.theme.colors.get.centralAttributes.centralShadowH,
          'centralshadowv -> customize.moduleContent.theme.colors.get.centralAttributes.centralShadowV,
          'centralshadowblur -> customize.moduleContent.theme.colors.get.centralAttributes.centralShadowBlur,
          'centralshadowspread -> customize.moduleContent.theme.colors.get.centralAttributes.centralShadowSpread,
          'centralshadowcolor -> customize.moduleContent.theme.colors.get.centralAttributes.centralShadowColor,
          'centralcornerradius -> customize.moduleContent.theme.colors.get.centralAttributes.centralCornerRadius,
          'mainbackcolor -> customize.moduleContent.theme.colors.get.mainAttributes.mainBackColor,
          'mainbacktransparent -> customize.moduleContent.theme.colors.get.mainAttributes.mainBackTransparent,
          'maincornerradius -> customize.moduleContent.theme.colors.get.mainAttributes.mainCornerRadius,
          'mainsepvisible -> customize.moduleContent.theme.colors.get.mainAttributes.mainSeparatorVisible,
          'mainsepcolor -> customize.moduleContent.theme.colors.get.mainAttributes.mainSeparatorColor
        ).executeUpdate()
      }
    }
  }

  /**
   * Load the budget of a wedding
   * @param weddingId the ID of the wedding
   * @return the budget if it exists
   */
  def loadCustomization(weddingId: Pk[Long]): Option[Customize] = {
    DB.withConnection { implicit connection =>
      loadCustomizationWithConnection(weddingId)
    }
  }

  /**
   * Load the budget of a wedding
   * @param weddingId the ID of the wedding
   * @return the budget if it exists
   */
  def loadCustomizationWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[Customize] = {
    SQL(
      """
      SELECT c.weddingid, c.fontid, c.themeid, wm.active
      FROM mod_customize c, weddingmodule wm
      WHERE
        c.weddingid = {weddingid} AND
        wm.weddingid = c.weddingid AND
        wm.moduleid = {moduleid}
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> Customize.ID
    ).as(customize.singleOpt)
  }

  /**
   * Load a customization font
   * @param id the font id
   * @return the font
   */
  def loadFont(id: Pk[Long]): CustomizeFont = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, name, parametervalue, size
        FROM mod_customize_font
        WHERE
          id = {id}
        """
      ).on(
        'id -> id
      ).as(font.single)
    }
  }

  /**
   * Load the default font
   * @return the font
   */
  def loadDefaultFont: CustomizeFont = {
    loadFont(Id(DEFAULT_FONT_ID))
  }

  /**
   * Load all available fonts
   * @return a list of fonts
   */
  def loadAvailableFonts: List[CustomizeFont] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, name, parametervalue, size
        FROM mod_customize_font
        """
      ).as(font *)
    }
  }

  /**
   * Load a customization theme
   * @param id the theme id
   * @param weddingId the wedding id
   * @return the theme
   */
  def loadTheme(id: Pk[Long], weddingId: Pk[Long]): CustomizeTheme = {
    loadTheme(id).copy(colors = loadColors(weddingId))
  }

  /**
   * Load a customization theme
   * @param id the theme id
   * @return the theme
   */
  def loadTheme(id: Pk[Long]): CustomizeTheme = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, name, path, haspicture
        FROM mod_customize_theme
        WHERE
          id = {id}
        """
      ).on(
        'id -> id
      ).as(theme.single)
    }
  }

  /**
   * Load the default theme
   * @return the theme
   */
  def loadDefaultTheme: CustomizeTheme = {
    loadTheme(Id(DEFAULT_THEME_ID))
  }

  /**
   * Load all available themes
   * @return a list of themes
   */
  def loadAvailableThemes: List[CustomizeTheme] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, name, path, haspicture
        FROM mod_customize_theme
        """
      ).as(theme *)
    }
  }

  /**
   * Delete all customization info for the given wedding
   * @param weddingId the wedding ID
   * @param customize the customization info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], customize: Customize)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM mod_customize_colors
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    SQL(
      """
      DELETE FROM mod_customize
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }


  /**
   * Load the colors of a custom theme
   * @param weddingId the wedding id
   * @return the colors
   */
  def loadColors(weddingId: Pk[Long]): Option[CustomizeThemeColors] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT weddingid,
          generalprimarybtnbackcolor, generalprimarybtnfontcolor, generalbtnfontcolor, generallinkfontcolor, generaltextfontcolor,
          menufontcolor, menubackcolor, menubacktransparent, menucornerradius,
          backgroundpicfilename, backgroundpiccontenttype, backgroundpicpos, backgroundbackcolor,
          centralbackpicfilename, centralbackpiccontenttype, centralbackpicpos, centralbackcolortop, centralbackcolorbottom, centralbacktransparent, centralshadowh, centralshadowv, centralshadowblur, centralshadowspread, centralshadowcolor, centralcornerradius,
          mainbackcolor, mainbacktransparent, maincornerradius, mainsepvisible, mainsepcolor
        FROM mod_customize_colors
        WHERE
          weddingid = {weddingid}
        """
      ).on(
        'weddingid -> weddingId
      ).as(colors.singleOpt)
    }
  }

  /**
   * Return the pre signed picture link for direct download (but with expiration date)
   * @param picture the picture to link
   * @return a string with the URL of the picture direct download
   */
  def getPictureLink(picture: CustomizePicture): Option[String] = {
    store.getPictureLink(picture)
  }

  /**
   * Insert a new picture
   * @param picture the picture info
   * @param file the picture file
   */
  def addPicture(picture: CustomizePicture, file: File) = {
    DB.withTransaction {
      implicit connection =>
        store.savePicture(picture, file)
        updatePicture(picture)
    }
  }

  /**
   * Delete the picture
   * @param picture the picture info
   */
  def deletePicture(picture: CustomizePicture) = {
    DB.withTransaction {
      implicit connection =>
        store.deletePicture(picture)
        updatePicture(picture)
    }
  }

  protected def updatePicture(picture: CustomizePicture)(implicit connection: Connection) = {
    val filenameFieldName = picture match {
      case pic: CustomizePictureBackground => "backgroundpicfilename"
      case pic: CustomizePictureCentral => "centralbackpicfilename"
      case pic: CustomizePictureLogo => "generallogopicfilename"
    }
    val contentTypeFieldName = picture match {
      case pic: CustomizePictureBackground => "backgroundpiccontenttype"
      case pic: CustomizePictureCentral => "centralbackpiccontenttype"
      case pic: CustomizePictureLogo => "generallogopiccontenttype"
    }

    SQL(
      """
      UPDATE mod_customize_colors SET
        """+filenameFieldName+""" = {filename},
        """+contentTypeFieldName+""" = {contenttype}
      WHERE
        weddingid = {weddingid}
      """
    ).on(
      'filename -> picture.getFilename,
      'contenttype -> picture.getContentType,
      'weddingid -> picture.getWeddingId
    ).executeUpdate()
  }
}