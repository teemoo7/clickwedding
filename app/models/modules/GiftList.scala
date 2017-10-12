package models.modules

import anorm._
import play.api.db.DB
import anorm.SqlParser._
import anorm.~
import anorm.Id
import play.api.Play.current
import collection.mutable
import java.util.Date
import models.authentication.User
import java.sql.Connection
import play.api.i18n.{Lang, Messages}

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 30.01.13
 * Time: 08:43
 * To change this template use File | Settings | File Templates.
 */
case class GiftList (weddingId: Pk[Long] = NotAssigned, moduleId: Pk[Long], moduleName: String, moduleContent: GiftListInfo, moduleActive: Boolean, movieUrl: Option[String], displayColumn: Option[Int] = None, displayPreviousModuleId: Option[Long], instructions: Option[String])
  extends Module(moduleId, moduleName, moduleContent, moduleActive, 0, movieUrl, Module.DISPLAY_TYPE_SMALL_AND_LARGE, Some(displayColumn.getOrElse(Module.DISPLAY_COLUMN_RIGHT)), displayPreviousModuleId, instructions = instructions) {
  override def init()(implicit connection: Connection) = {
    GiftList.addGiftList(this)
  }
  override def deleteWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    GiftList.deleteModuleForWedding(weddingId, this)
  }

  override def title(wedding: models.wedding.Wedding)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): Option[String] =
    Some(Messages("main.modules.giftList.giftList"))

  override def display(wedding: models.wedding.Wedding, isSmall: Boolean)(implicit request: play.api.mvc.RequestHeader, flash: play.api.mvc.Flash, user: Option[models.authentication.User], lang: Lang): play.api.templates.Html = {
    wedding.modules.find(module => module.getId.get == models.modules.GiftList.ID) match {
      case Some(module) =>
        module match {
          case giftList: models.modules.GiftList =>
            views.html.modules.giftList.displaySmall(wedding, giftList.moduleContent)
        }
      case _ =>
        views.html.modules.notFound(wedding)
    }
  }
}
case class GiftListInfo(weddingId: Pk[Long], paypalInfo: GiftListPayPalInfo, email: Option[String], currency: String, items: mutable.HashMap[Long, GiftListItem])
case class GiftListItem(id: Pk[Long], weddingId: Pk[Long], description: String, unitPrice: Double, stock: Option[Int], isActive: Boolean) {
  override def toString() = {
    "GiftListItem("+ (id, weddingId, description, unitPrice, stock, isActive) mkString(", ") + ")"
  }
}

case class GiftListPayPalInfo(accountId: Option[String], status: Int, statusDate: Date, token: Option[String], secret: Option[String], fullName: Option[String])

case class Cart(items: mutable.HashMap[Long, CartItem]) {
  override def toString() = {
    "Cart("+ (items.values.map(cartItem => cartItem.toString) mkString(", ")) + ")"
  }
}
case class CartItem(item: GiftListItem, number: Int) {
  def total: Double = number.toDouble * item.unitPrice
  override def toString() = {
    "CartItem("+ (number, item.toString) mkString(", ") + ")"
  }
}

case class GiftListPurchase(id: Pk[Long], weddingId: Pk[Long], date: Date, name: String, message: Option[String], email: Option[String],
                            token: Option[String], transactionId: Option[String], correlationId: Option[String], payerId: Option[String],
                            accountId: Option[String], cart: Cart, currency: String, totalCost: Double, fees: Double) {
  override def toString() = {
    "GiftListPurchase("+ (Seq(id, weddingId, date, name, message, email, token, transactionId, correlationId, payerId, accountId, totalCost, currency, fees, cart.toString) mkString(", ")) + ")"
  }
}

object GiftList {
  val ID = 6
  val NAME = "GiftList"

  val PAYPAL_PERMISSIONS_STATUS_NOT_WORKING = 0
  val PAYPAL_PERMISSIONS_STATUS_NOT_VERIFIED = 1
  val PAYPAL_PERMISSIONS_STATUS_VERIFIED = 2

  /**
   * Parse gift list info and its items from a ResultSet
   */
  val giftList = {
    get[Pk[Long]]("weddingid") ~
      get[Option[String]]("email") ~
      get[String]("currency") ~
      get[Boolean]("active") ~
      get[Option[Int]]("displaycolumn") ~
      get[Option[Long]]("displayprevious") ~
      get[Option[String]]("instructions") map {
      case weddingId ~ email ~ currency ~ active ~ displayColumn ~ displayPreviousModuleId ~ instructions
      => GiftList(weddingId, Id(GiftList.ID), GiftList.NAME, GiftListInfo(weddingId, getPayPalAccountInfo(weddingId), email, currency, getGiftListItems(weddingId)), active, None, displayColumn, displayPreviousModuleId, instructions)
    }
  }

  /**
   * Parse gift item from a ResultSet
   */
  val giftListItem = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingid") ~
      get[String]("description") ~
      get[Double]("unitprice") ~
      get[Boolean]("active") ~
      get[Option[Int]]("quantity") map {
      case id ~ weddingId ~ description ~ unitPrice ~ isActive ~ stock
      => GiftListItem(id, weddingId, description, unitPrice, stock, isActive)
    }
  }

  /**
   * Parse gift list purchase from a ResultSet
   */
  val giftListPurchase = {
    get[Pk[Long]]("id") ~
      get[Pk[Long]]("weddingid") ~
      get[Date]("date") ~
      get[String]("name") ~
      get[Option[String]]("message") ~
      get[Option[String]]("email") ~
      get[Option[String]]("token") ~
      get[Option[String]]("transactionid") ~
      get[Option[String]]("correlationid") ~
      get[Option[String]]("payerid") ~
      get[Option[String]]("accountid") ~
      get[String]("currency") ~
      get[Double]("totalcost") ~
      get[Double]("fees") map {
      case id ~ weddingId ~ date ~ name ~ message ~ email ~ token ~ transactionId ~ correlationId ~ payerId ~ accountId ~ currency ~ totalCost ~ fees
      => GiftListPurchase(id, weddingId, date, name, message, email, token, transactionId, correlationId, payerId, accountId, getGiftListPurchaseCart(id), currency, totalCost, fees)
    }
  }

  /**
   * Parse gift list cart item from a ResultSet
   */
  val giftListPurchaseCartItem = {
    get[Pk[Long]]("purchaseid") ~
      get[Pk[Long]]("itemid") ~
      get[Int]("quantity") ~
      get[Double]("unitprice") ~
      get[Pk[Long]]("weddingid") ~
      get[String]("description") ~
      get[Boolean]("active") map {
      case purchaseId ~ itemId ~ quantity ~ unitprice ~ weddingId ~ description ~ active
      => CartItem(GiftListItem(itemId, weddingId, description, unitprice, None, active), quantity)
    }
  }

  /**
   * Parse PaPyal account info from a ResultSet
   */
  val payPalAccountInfo = {
    get[Pk[Long]]("weddingid") ~
      get[Option[String]]("accountid") ~
      get[Int]("status") ~
      get[Date]("statusdate") ~
      get[Option[String]]("token") ~
      get[Option[String]]("secret") ~
      get[Option[String]]("fullname") map {
      case weddingId ~ accountId ~ status ~ statusDate ~ token ~ secret ~ fullName
      => GiftListPayPalInfo(accountId, status, statusDate, token, secret, fullName)
    }
  }

  /**
   * Insert a new gift list for the given wedding
   * @param giftList the initial value for the gist list
   */
  def addGiftList(giftList: GiftList)(implicit connection: Connection) = {
    SQL(
      """
      INSERT INTO mod_giftList (
        weddingid, email, currency
      ) VALUES (
        {weddingid}, {email}, {currency}
      )
      """
    ).on(
      'weddingid -> giftList.weddingId,
      'email -> giftList.moduleContent.email,
      'currency -> giftList.moduleContent.currency
    ).executeUpdate()

    SQL(
      """
      INSERT INTO mod_giftList_paypal (
        weddingid, accountid, status, statusdate, token, secret, fullname
      ) VALUES (
        {weddingid}, {accountid}, {status}, {statusdate}, {token}, {secret}, {fullname}
      )
      """
    ).on(
      'weddingid -> giftList.weddingId,
      'accountid -> giftList.moduleContent.paypalInfo.accountId,
      'status -> giftList.moduleContent.paypalInfo.status,
      'statusdate -> giftList.moduleContent.paypalInfo.statusDate,
      'token -> giftList.moduleContent.paypalInfo.token,
      'secret -> giftList.moduleContent.paypalInfo.secret,
      'fullname -> giftList.moduleContent.paypalInfo.fullName
    ).executeUpdate()
  }

  /**
   * Update the gift list info for a given wedding
   * @param giftList the gift list
   */
  def editGiftList(giftList: GiftList) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_giftList SET
              email = {email}, currency = {currency}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> giftList.weddingId,
          'email -> giftList.moduleContent.email,
          'currency -> giftList.moduleContent.currency
        ).executeUpdate()
    }
  }

  /**
   * Update the gift list PayPal info for a given wedding
   * @param payPalInfo the PayPal info
   */
  def editGiftListPayPalInfo(weddingId: Pk[Long], payPalInfo: GiftListPayPalInfo) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE mod_giftList_paypal SET
              accountid = {accountid}, status = {status}, statusdate = {statusdate}, token = {token}, secret = {secret}, fullname = {fullname}
            WHERE
              weddingid = {weddingid}
          """
        ).on(
          'weddingid -> weddingId,
          'accountid -> payPalInfo.accountId,
          'status -> payPalInfo.status,
          'statusdate -> payPalInfo.statusDate,
          'token -> payPalInfo.token,
          'secret -> payPalInfo.secret,
          'fullname -> payPalInfo.fullName
        ).executeUpdate()
    }
  }

  /**
   * Update the gift list instructions for a given wedding
   * @param giftList the gift list module
   */
  def editGiftListInstructions(giftList: GiftList) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            UPDATE weddingmodule SET
              instructions = {instructions}
            WHERE
              weddingid = {weddingid} AND
              moduleid = {moduleid}
          """
        ).on(
          'instructions -> giftList.instructions,
          'weddingid -> giftList.weddingId,
          'moduleid -> giftList.moduleId
        ).executeUpdate()
    }
  }

  /**
   * Load the gift list info of a wedding
   * @param weddingId the ID of the wedding
   * @return the gift list if it exists
   */
  def loadGiftListWithConnection(weddingId: Pk[Long])(implicit connection: Connection): Option[GiftList] = {
    SQL(
      """
      SELECT g.weddingid, g.email, g.currency, wm.active, wm.displaycolumn, wm.displayprevious, wm.instructions
      FROM mod_giftList g, weddingmodule wm
      WHERE
        g.weddingid = {weddingid} AND
        wm.weddingid = g.weddingid AND
        wm.moduleid = {moduleid}
      """
    ).on(
      'weddingid -> weddingId,
      'moduleid -> GiftList.ID
    ).as(giftList.singleOpt)
  }

  /**
   * Return the list of active gift items for the wedding
   * @param weddingId the ID of the wedding
   * @return a list of gift items
   */
  def getGiftListItems(weddingId: Pk[Long]): mutable.HashMap[Long, GiftListItem] = {
    DB.withConnection { implicit connection =>
      val listItems =
        SQL(
          """
          SELECT i.id, i.weddingid, i.description, i.unitprice, i.active, s.quantity
          FROM mod_giftlist_item i, mod_giftlist_item_stock s
          WHERE
            i.weddingid = {weddingid} AND
            s.itemid = i.id AND
            i.active = 1
          """
        ).on(
          'weddingid -> weddingId
        ).as(giftListItem *)
      val map = mutable.HashMap[Long, GiftListItem]()
      for (item <- listItems) {
        map.put(item.id.get, item)
      }
      map
    }
  }

  /**
   * Return the list of all gift items for the wedding
   * @param weddingId the ID of the wedding
   * @return a list of gift items
   */
  def getAllGiftListItems(weddingId: Pk[Long])(implicit connection: Connection): List[GiftListItem] = {
    SQL(
      """
      SELECT i.id, i.weddingid, i.description, i.unitprice, i.active, s.quantity
      FROM mod_giftlist_item i, mod_giftlist_item_stock s
      WHERE
        i.weddingid = {weddingid} AND
        s.itemid = i.id
      """
    ).on(
      'weddingid -> weddingId
    ).as(giftListItem *)
  }

  /**
   * Return the PayPal info for the given wedding gift list
   * @param weddingId the wedding ID
   * @return the PayPal info
   */
  def getPayPalAccountInfo(weddingId: Pk[Long]): GiftListPayPalInfo = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT weddingid, accountid, status, statusdate, token, secret, fullname
        FROM mod_giftList_paypal
        WHERE
          weddingid = {weddingid}
        """
      ).on(
        'weddingid -> weddingId
      ).as(payPalAccountInfo.single)
    }
  }

  /**
   * Insert a new gift item for the given wedding
   * @param giftListItem the value for the new marker
   */
  def addGiftListItem(giftListItem: GiftListItem) = {
    DB.withTransaction {
      implicit connection =>
        val id: Option[Long] = SQL(
        """
          INSERT INTO mod_giftlist_item (
            weddingid, description, unitprice, active
          ) VALUES (
            {weddingid}, {description}, {unitprice}, {active}
          )
        """
      ).on(
        'weddingid -> giftListItem.weddingId,
        'description -> giftListItem.description,
        'unitprice -> giftListItem.unitPrice,
        'active -> true
      ).executeInsert()

      SQL(
        """
          INSERT INTO mod_giftlist_item_stock (
            itemid, quantity
          ) VALUES (
            {itemid}, {quantity}
          )
        """
      ).on(
        'itemid -> id.get,
        'quantity -> giftListItem.stock
      ).executeUpdate()

      giftListItem.copy(id = Id(id.get))
    }
  }

  /**
   * Update the gist list item
   * @param giftListItem the item
   */
  def editGiftListItem(giftListItem: GiftListItem) = {
    DB.withTransaction { implicit connection =>
      SQL(
        """
          UPDATE mod_giftlist_item SET
            description = {description}, unitprice = {unitprice}, active = {active}
          WHERE
            id = {id}
        """
      ).on(
        'description -> giftListItem.description,
        'unitprice -> giftListItem.unitPrice,
        'active -> giftListItem.isActive,
        'id -> giftListItem.id
      ).executeUpdate()

      SQL(
        """
          UPDATE mod_giftlist_item_stock SET
            quantity = {quantity}
          WHERE
            itemid = {itemid}
        """
      ).on(
        'quantity -> giftListItem.stock,
        'itemid -> giftListItem.id
      ).executeUpdate()

      giftListItem
    }
  }

  /**
   * Set the gist list item as inactive
   * @param id the id of the gift
   */
  def inactiveGiftListItem(id: Pk[Long]) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          UPDATE mod_giftlist_item SET
            active = {active}
          WHERE
            id = {id}
        """
      ).on(
        'active -> false,
        'id -> id
      ).executeUpdate()
    }
  }

  /**
   * Delete a gift item
   * @param id the id of the gift
   */
  def deleteGiftListItem(id: Pk[Long]) = {
    DB.withTransaction { implicit connection =>
      SQL(
        """
        DELETE FROM mod_giftlist_item_stock
        WHERE
         itemid = {itemid}
        """
      ).on(
        'itemid -> id
      ).executeUpdate()

      SQL(
        """
        DELETE FROM mod_giftlist_item
        WHERE
         id = {id}
        """
      ).on(
        'id -> id
      ).executeUpdate()
    }
  }

  /**
   * Insert the purchase global info, and the gifts bought
   * @param wedding the wedding
   * @param purchase the gift list purchase
   * @return
   */
  def addGiftListPurchase(wedding: models.wedding.Wedding, purchase: GiftListPurchase) = {
    DB.withTransaction {
      implicit connection =>
        val id: Option[Long] = SQL(
          """
          INSERT INTO mod_giftlist_purchase (
            weddingid, date, name, message, email, token, transactionid, correlationid, payerid, accountid, currency, totalcost, fees
          ) VALUES (
            {weddingid}, {date}, {name}, {message}, {email}, {token}, {transactionid}, {correlationid}, {payerid}, {accountid}, {currency}, {totalcost}, {fees}
          )
          """
        ).on(
          'weddingid -> purchase.weddingId,
          'date -> purchase.date,
          'name -> purchase.name,
          'message -> purchase.message,
          'email -> purchase.email,
          'token -> purchase.token,
          'transactionid -> purchase.transactionId,
          'correlationid -> purchase.correlationId,
          'payerid -> purchase.payerId,
          'accountid -> purchase.accountId,
          'currency -> purchase.currency,
          'totalcost -> purchase.totalCost,
          'fees -> purchase.fees
        ).executeInsert()

        val purchaseId = id.get

        for (cartItem <- purchase.cart.items.values) {
          SQL(
            """
            INSERT INTO mod_giftlist_item_purchase (
              itemid, purchaseid, quantity, unitprice
            ) VALUES (
              {itemid}, {purchaseid}, {quantity}, {unitprice}
            )
            """
          ).on(
            'itemid -> cartItem.item.id,
            'purchaseid -> purchaseId,
            'quantity -> cartItem.number,
            'unitprice -> cartItem.item.unitPrice
          ).executeUpdate()
        }
        purchase.copy(id = Id(purchaseId))
    }
  }

  /**
   * Load all the gift list purchases that were processed for the wedding
   * @param weddingId the wedding ID
   * @return a list of purchases
   */
  def getAllGiftListPurchases(weddingId: Pk[Long]): List[GiftListPurchase] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, date, name, message, email, token, transactionid, correlationid, payerid, accountid, currency, totalcost, fees
        FROM mod_giftlist_purchase
        WHERE
          weddingid = {weddingid}
        ORDER BY date DESC
        """
      ).on(
        'weddingid -> weddingId
      ).as(giftListPurchase*)
    }
  }

  /**
   * Load all the items (gifts) of a gift list purchase and return them as a cart
   * @param purchaseId the purchase ID
   * @return a cart of gifts
   */
  def getGiftListPurchaseCart(purchaseId: Pk[Long]): Cart = {
    DB.withConnection { implicit connection =>
      val cartItems = SQL(
        """
          SELECT ip.itemid, ip.purchaseid, ip.quantity, ip.unitprice, i.weddingid, i.description, i.active
          FROM mod_giftlist_item_purchase ip, mod_giftlist_item i
          WHERE
            purchaseid = {purchaseid} AND
            i.id = ip.itemid
        """
      ).on(
        'purchaseid -> purchaseId
      ).as(giftListPurchaseCartItem*)

      val map = mutable.HashMap[Long, CartItem]()
      for (cartItem <- cartItems) {
        map.put(cartItem.item.id.get, cartItem)
      }
      Cart(map)
    }
  }

  /**
   * Load the required gift list purchases
   * @param weddingId the wedding ID
   * @param purchaseId the purchase ID
   * @return the complete purchase
   */
  def getGiftListPurchase(weddingId: Pk[Long], purchaseId: Long): Option[GiftListPurchase] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
        SELECT id, weddingid, date, name, message, email, token, transactionid, correlationid, payerid, accountid, currency, totalcost, fees
        FROM mod_giftlist_purchase
        WHERE
          weddingid = {weddingid} AND
          id = {id}
        """
      ).on(
        'weddingid -> weddingId,
        'id -> purchaseId
      ).as(giftListPurchase.singleOpt)
    }
  }

  /**
   * Delete all gift list info for the given wedding
   * @param weddingId the wedding ID
   * @param giftList the gift list info
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteModuleForWedding(weddingId: Pk[Long], giftList: GiftList)(implicit connection: Connection) = {
    for (item <- getAllGiftListItems(weddingId)) {
      SQL(
        // Items purchase
        """
        DELETE FROM mod_giftlist_item_purchase
        WHERE
        itemid = {itemid}
        """
      ).on(
        'itemid -> item.id
      ).executeUpdate()
      // Items stock
      SQL(
        """
        DELETE FROM mod_giftlist_item_stock
        WHERE
        itemid = {itemid}
      """
      ).on(
        'itemid -> item.id
      ).executeUpdate()
    }
    // Items
    SQL(
      """
      DELETE FROM mod_giftlist_item
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    // Purchases
    SQL(
      """
      DELETE FROM mod_giftlist_purchase
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    // PayPal
    SQL(
      """
      DELETE FROM mod_giftlist_paypal
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    // GiftList
    SQL(
      """
      DELETE FROM mod_giftlist
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }
}