package models.providers

import anorm._
import java.util.{Calendar, Date}
import anorm.SqlParser._
import play.api.db.DB
import anorm.~
import play.api.Play.current
import models.authentication.User
import java.sql.Connection
import play.api.libs.json._
import java.io.File
import com.typesafe.plugin._
import scala.Some
import anorm.~
import anorm.Id
import controllers.providers.ProviderPicturePlugin

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.09.13
 * Time: 10:49
 * To change this template use File | Settings | File Templates.
 */
case class Provider(id: Pk[Long], title: String, slogan: Option[String],
                    name: String, street: String, streetNb: Option[String], place: String, zip: String, country: String,
                    phone: Option[String], email: String, website: Option[String],
                    logo: Option[ProviderLogoPicture], showroom: List[ProviderShowroomPicture], description: Option[String],
                    latitude: Double, longitude: Double, categoryId: Long, isValid: Boolean, since: Date,
                    currentPack: Option[ProviderPack]) {
  def hasLogo: Boolean = logo.isDefined
  def hasShowroom: Boolean = showroom.nonEmpty
  def isShowroomQuotaReached: Boolean = showroom.size >= getMaxPicturesShowroom
  def getMaxPicturesShowroom: Int = currentPack match {
    case Some(pack) => {
      pack.packType.nbPicturesShowroom
    }
    case None => 0
  }
}

case class ProviderStats(providerId: Pk[Long], displayStats: Option[ProviderStatsEventTotal], detailStats: Option[ProviderStatsEventTotal], mailStats: Option[ProviderStatsEventTotal], websiteStats: Option[ProviderStatsEventTotal])

case class ProviderStatsEvent(providerId: Pk[Long], date: Date, IPAddress: String, eventType: Int)

case class ProviderStatsEventTotal(providerId: Pk[Long], nb: Long = 0, eventType: Int)

case class ProviderCategory(categoryId: Pk[Long], name: String)

case class ProviderPack(packType: ProviderPackType, purchase: ProviderPurchase)

case class ProviderPackType(id: Pk[Long], name: String, nbMonths: Int, price: Double, currency: String, isAvailable: Boolean, nbUsage: Option[Int], nbPicturesShowroom: Int)

case class ProviderPurchase(id: Pk[Long], providerId: Long, packId: Long, since: Date, until: Date, amount: Double, currency: String, paid: Boolean, paymentDate: Option[Date], paymentMethod: Option[Int], paymentReference: Option[String], transaction: Option[ProviderTransaction]) {
  /**
   * Give the purchase invoice deadline, which is the "since" date plus 30 days
   * @return the invoice deadline date
   */
  def deadline: Date = {
    val cal = Calendar.getInstance()
    cal.setTime(since)
    cal.add(Calendar.DATE, 30)
    cal.getTime()
  }

  /**
   * Give the invoice #, given its ID and date
   * @return invoice number
   */
  def invoiceNumber: String = {
    new java.text.SimpleDateFormat("yyyy").format(since)+"/"+id.get
  }

  /**
   * Give the money transfer message (reference) for thie current purchase
   * @return
   */
  def moneyTransferReference: String = {
    "CW"+new java.text.SimpleDateFormat("yyyy").format(since)+"/"+id.get
  }
}

case class ProviderTransaction(purchaseId: Long, amount: Double, currency: String, token: Option[String], correlationId: Option[String], payerId: Option[String], transactionId: Option[String], accountId: Option[String])


case class ProviderLogoPicture(providerId: Long, filename: Option[String], contentType: Option[String]) {
  def key: String = {
    providerId.toString
  }
}

case class ProviderShowroomPicture(id: Pk[Long], providerId: Long, filename: Option[String], size: Int, date: Date, contentType: Option[String], title: Option[String]) {
  def key: String = {
    providerId.toString+"/"+id.get.toString
  }
  def thumbKey: String = {
    providerId.toString+"/thumb/"+id.get.toString
  }
}

object Provider {
  val STATS_EVENT_TYPE_DISPLAY = 0
  val STATS_EVENT_TYPE_DETAIL = 1
  val STATS_EVENT_TYPE_MAIL = 2
  val STATS_EVENT_TYPE_WEBSITE = 3
  val STATS_EVENT_TYPES = List(STATS_EVENT_TYPE_DISPLAY, STATS_EVENT_TYPE_DETAIL, STATS_EVENT_TYPE_MAIL, STATS_EVENT_TYPE_WEBSITE)

  val PAYMENT_METHOD_FREE = 0
  val PAYMENT_METHOD_PAYPAL = 1
  val PAYMENT_METHOD_EBANKING = 2
  val PAYMENT_METHOD_CASH = 3

  val LOGO_PICTURE_WIDTH = 150
  val LOGO_PICTURE_HEIGHT = 150
  val SHOWROOM_PICTURE_WIDTH = 1024
  val SHOWROOM_PICTURE_HEIGHT = 768
  val SHOWROOM_THUMBNAIL_WIDTH = 150
  val SHOWROOM_THUMBNAIL_HEIGHT = 150

  /**
   * Images store definition
   */
  val store = use[ProviderPicturePlugin]

  val provider = {
    get[Pk[Long]]("id") ~
      get[String]("title") ~
      get[Option[String]]("slogan") ~
      get[String]("name") ~
      get[String]("street") ~
      get[Option[String]]("streetnb") ~
      get[String]("place") ~
      get[String]("zip") ~
      get[String]("country") ~
      get[Option[String]]("phone") ~
      get[String]("email") ~
      get[Option[String]]("website") ~
      get[Boolean]("haslogo") ~
      get[Option[String]]("description") ~
      get[Double]("latitude") ~
      get[Double]("longitude") ~
      get[Long]("catid") ~
      get[Boolean]("isvalid") ~
      get[Date]("since") map {
      case id ~ title ~ slogan ~ name ~ street ~ streetNb ~ place ~ zip ~ country ~ phone ~ email ~ website ~ hasLogo ~ description ~ latitude ~ longitude ~ categoryId ~ isValid ~ since
        => Provider(id, title, slogan, name, street, streetNb, place, zip, country, phone, email, website, hasLogo match {case true => getLogo(id.get) case false => None}, getPictures(id.get), description, latitude, longitude, categoryId, isValid, since, getCurrentPack(id))
    }
  }

  val packType = {
    get[Pk[Long]]("id") ~
      get[String]("name") ~
      get[Int]("nbmonths") ~
      get[Double]("price") ~
      get[String]("currency") ~
      get[Boolean]("isavailable") ~
      get[Option[Int]]("nbusage") ~
      get[Int]("nbpicturesshowroom") map {
      case id ~ name ~ nbMonths ~ price ~ currency ~ isAvailable ~ nbUsage ~ nbPucruresShowroom => ProviderPackType(id, name, nbMonths, price, currency, isAvailable, nbUsage, nbPucruresShowroom)
    }
  }

  val purchase = {
    get[Pk[Long]]("id") ~
      get[Long]("providerid") ~
      get[Long]("packid") ~
      get[Date]("since") ~
      get[Date]("until") ~
      get[Double]("amount") ~
      get[String]("currency") ~
      get[Boolean]("paid") ~
      get[Option[Date]]("paymentdate") ~
      get[Option[Int]]("paymentmethod") ~
      get[Option[String]]("paymentreference") map {
      case id ~ providerId ~ packId ~ since ~ until ~ amount ~ currency ~ paid ~ paymentDate ~ paymentMethod ~ paymentReference =>
        ProviderPurchase(id, providerId, packId, since, until, amount, currency, paid, paymentDate, paymentMethod, paymentReference, getTransaction(id.get))
    }
  }

  val logo = {
    get[Long]("providerid") ~
      get[Option[String]]("filename") ~
      get[Option[String]]("contenttype") map {
      case providerId ~ filename ~ contentType => ProviderLogoPicture(providerId, filename, contentType)
    }
  }

  val picture = {
    get[Pk[Long]]("id") ~
      get[Long]("providerid") ~
      get[Option[String]]("filename") ~
      get[Int]("size") ~
      get[Date]("date") ~
      get[Option[String]]("contenttype") ~
      get[Option[String]]("title") map {
      case id ~ providerId ~ filename ~ size ~ date ~ contentType ~ title => ProviderShowroomPicture(id, providerId, filename, size, date, contentType, title)
    }
  }

  val statsEventTotal = {
    get[Long]("nb") ~
      get[Long]("providerid") ~
      get[Int]("eventtype") map {
      case nb ~ providerId ~ eventType => ProviderStatsEventTotal(Id(providerId), nb, eventType)
    }
  }

  val category = {
    get[Pk[Long]]("id") ~
      get[String]("name") map {
      case id ~ name => ProviderCategory(id, name)
    }
  }

  val transaction = {
    get[Long]("purchaseid") ~
      get[Double]("amount") ~
      get[String]("currency") ~
      get[Option[String]]("token") ~
      get[Option[String]]("correlationid") ~
      get[Option[String]]("payerid") ~
      get[Option[String]]("transactionid") ~
      get[Option[String]]("accountid") map {
      case purchaseId ~ amount ~ currency ~ token ~ correlationId ~ payerId ~ transactionId ~ accountId => ProviderTransaction(purchaseId, amount, currency, token, correlationId, payerId, transactionId, accountId)
    }
  }

  /**
   * Converter from Provider to JSON object
   */
  implicit val providerWrites = new Writes[Provider] {
    def writes(p: Provider): JsValue = {
      Json.obj(
        "id" -> p.id.get,
        "title" -> p.title,
        "slogan" -> p.slogan,
        "name" -> p.name,
        "street" -> p.street,
        "streetNb" -> p.streetNb,
        "zip" -> p.zip,
        "place" -> p.place,
        "country" -> p.country,
        "website" -> p.website,
        "websiteUrl" -> controllers.helpers.UtilsHelper.removeMethodToUrl(p.website.getOrElse("")),
        "phone" -> p.phone,
        "latitude" -> p.latitude,
        "longitude" -> p.longitude,
        "logoUrl" -> (
          p.logo match {
            case Some(logoPicture) => {
              Provider.getProviderLogoPictureLink(logoPicture) match {
                case Some(link) => link
                case None => ""
              }
            }
            case None => {
              ""
            }
          }
        ),
        "showroom" -> (if (p.showroom.size>0) p.showroom.size else JsNull)
      )
    }
  }

  /**
   * Find a provider given its ID
   * @param id the provider id
   * @return the provider if found
   */
  def getProvider(id: Pk[Long]): Option[Provider] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT  id, title, slogan,
                  name, street, streetnb, place, zip, country,
                  phone, email, website,
                  haslogo, description, tags,
                  latitude, longitude, catid, isvalid, since
          FROM service_provider WHERE id = {id}
          """
        ).on(
          'id -> id
        ).as(provider.singleOpt)
    }
  }

  /**
   * Find all providers
   * @return a list of providers
   */
  def getAllProviders: List[Provider] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT  id, title, slogan,
                  name, street, streetnb, place, zip, country,
                  phone, email, website,
                  haslogo, description, tags,
                  latitude, longitude, catid, isvalid, since
          FROM service_provider
          WHERE isvalid = 1
          """
        ).as(provider *)
    }
  }

  /**
   * Find all providers for the given category
   * @return a list of providers
   */
  def getAllProvidersForCategory(categoryId: Long): List[Provider] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT  id, title, slogan,
                  name, street, streetnb, place, zip, country,
                  phone, email, website,
                  haslogo, description, tags,
                  latitude, longitude, catid, isvalid, since
          FROM service_provider
          WHERE isvalid = 1 AND catid = {categoryid}
          """
        ).on(
          'categoryid -> categoryId
        ).as(provider *)
    }
  }

  /**
   * Find all providers for a user
   * @param user the user
   * @return a list of providers
   */
  def getProvidersByUserId(user: User): List[Provider] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT  id, title, slogan,
                  name, street, streetnb, place, zip, country,
                  phone, email, website,
                  haslogo, description, tags,
                  latitude, longitude, catid, isvalid, since
          FROM service_provider
          WHERE userid = {userid}
          """
        ).on(
          'userid -> user.userId
        ).as(provider *)
    }
  }

  /**
   * Create a new provider given its attributes
   * @param provider the new provider
   */
  def addProvider(provider: Provider, user: User): Provider = {
    DB.withTransaction {
      implicit connection =>

        // 1. Create a new provider (no payment info)
        val providerId: Option[Long] = SQL(
          """
          INSERT INTO service_provider (
            title, slogan,
            name, street, streetnb, place, zip, country,
            phone, email, website,
            haslogo, description,
            latitude, longitude, catid, isvalid, since, userid
          ) VALUES (
            {title}, {slogan},
            {name}, {street}, {streetnb}, {place}, {zip}, {country},
            {phone}, {email}, {website},
            {haslogo}, {description},
            {latitude}, {longitude}, {catid}, {isvalid}, {since}, {userid}
          )
          """
        ).on(
          'title -> provider.title,
          'slogan -> provider.slogan,
          'name -> provider.name,
          'street -> provider.street,
          'streetnb -> provider.streetNb,
          'place -> provider.place,
          'zip -> provider.zip,
          'country -> provider.country,
          'phone -> provider.phone,
          'email -> provider.email,
          'website -> provider.website,
          'haslogo -> false,
          'description -> provider.description,
          'latitude -> provider.latitude,
          'longitude -> provider.longitude,
          'catid -> provider.categoryId,
          'isvalid -> false,
          'since -> new Date(),
          'userid -> user.userId
        ).executeInsert()
      val providerWithId = provider.copy(id = Id(providerId.get))

      // 2. Add a purchase info for the current pack (but it might not be paid yet!)
      val purchaseId: Option[Long] = SQL(
        """
        INSERT INTO service_provider_purchase (
          providerid, packid, amount, currency, since, until, paid, paymentdate, paymentmethod, paymentreference
        ) VALUES (
          {providerid}, {packid}, {amount}, {currency}, {since}, {until}, {paid}, {paymentdate}, {paymentmethod}, {paymentreference}
        )
        """
      ).on(
        'providerid -> providerWithId.id,
        'packid -> providerWithId.currentPack.get.packType.id,
        'amount -> providerWithId.currentPack.get.purchase.amount,
        'currency -> providerWithId.currentPack.get.purchase.currency,
        'since -> providerWithId.currentPack.get.purchase.since,
        'until -> providerWithId.currentPack.get.purchase.until,
        'paid ->  providerWithId.currentPack.get.purchase.paid,
        'paymentdate ->  providerWithId.currentPack.get.purchase.paymentDate,
        'paymentmethod ->  providerWithId.currentPack.get.purchase.paymentMethod,
        'paymentreference ->  providerWithId.currentPack.get.purchase.paymentReference
      ).executeInsert()

      providerWithId.copy(currentPack = Some(providerWithId.currentPack.get.copy(purchase = providerWithId.currentPack.get.purchase.copy(id = Id(purchaseId.get)))))
    }
  }

  /**
   * Set the purchase payment info (i.e. the purchase has been paid)
   * @param purchase the purchase info
   */
  def updatePurchasePayment(purchase: ProviderPurchase) = {
    DB.withConnection {
      implicit connection =>
        updatePurchasePaymentWithConnection(purchase)
    }
  }

  /**
   * Set the purchase payment info (i.e. the purchase has been paid) inside an SQL connection (transaction)
   * @param purchase the purchase info
   */
  protected def updatePurchasePaymentWithConnection(purchase: ProviderPurchase)(implicit connection: Connection) = {
      SQL(
        """
        UPDATE service_provider_purchase
        SET
          paid = {paid},
          paymentdate = {paymentdate},
          paymentmethod = {paymentmethod},
          paymentreference = {paymentreference}
        WHERE id = {id}
        """
      ).on(
        'id -> purchase.id,
        'paid -> purchase.paid,
        'paymentdate -> purchase.paymentDate,
        'paymentmethod -> purchase.paymentMethod,
        'paymentreference -> purchase.paymentReference
      ).executeUpdate()
  }

  /**
   * Update the provider info for validity (which means that only admin should use this method)
   * @param providerId the provider ID
   * @param isValid is the provider valid?
   */
  def editProviderValidity(providerId: Pk[Long], isValid: Boolean) {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE service_provider
          SET
            isvalid = {isvalid}
          WHERE id = {id}
          """
        ).on(
          'id -> providerId,
          'isvalid -> isValid
        ).executeUpdate()
    }
  }

  /**
   * Update the provider basic info (not admin attributes, which means that this method can be used by provider itself)
   * @param provider the provider
   */
  def editProvider(provider: Provider) {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE service_provider
          SET
            title = {title}, slogan = {slogan},
            name = {name}, street = {street}, streetnb = {streetnb}, place = {place}, zip = {zip}, country = {country},
            phone = {phone}, email = {email}, website = {website},
            description = {description},
            latitude = {latitude}, longitude = {longitude}, catid = {catid}
          WHERE id = {id}
          """
        ).on(
          'id -> provider.id,
          'title -> provider.title,
          'slogan -> provider.slogan,
          'name -> provider.name,
          'street -> provider.street,
          'streetnb -> provider.streetNb,
          'place -> provider.place,
          'zip -> provider.zip,
          'country -> provider.country,
          'phone -> provider.phone,
          'email -> provider.email,
          'website -> provider.website,
          'description -> provider.description,
          'latitude -> provider.latitude,
          'longitude -> provider.longitude,
          'catid -> provider.categoryId
        ).executeUpdate()
    }
  }

  /**
   * Try to load the current package for the given provider, is any
   * @param providerId the provider ID
   * @return a provider pack with purchase and pack type if found
   */
  def getCurrentPack(providerId: Pk[Long]): Option[ProviderPack] = {
    DB.withConnection {
      implicit connection =>
        val currentPurchaseOpt = SQL(
          """
          SELECT id, providerid, packid, since, until, amount, currency, paid, paymentdate, paymentmethod, paymentreference
          FROM service_provider_purchase
          WHERE
            providerid = {providerid} AND
            (since IS NULL OR since <= NOW()) AND
            (until IS NULL OR until >= NOW())
          """
        ).on(
          'providerid -> providerId
        ).as(purchase.singleOpt)

        currentPurchaseOpt match {
          case Some(currentPurchase) => {
            val currentPackType = SQL(
              """
              SELECT id, name, nbmonths, price, currency, isavailable, nbusage, nbpicturesshowroom
              FROM service_provider_pack WHERE id = {id}
              """
            ).on(
              'id -> currentPurchase.packId
            ).as(packType.single)
            Some(ProviderPack(currentPackType, currentPurchase))
          }
          case None => None
        }
    }
  }

  /**
   * Retrieve all the available pack types (unavailable does not mean invalid, but not currently not allowed
   * to be displayed)
   * @return a list a pack types
   */
  def getAvailablePackTypes: List[ProviderPackType] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, name, nbmonths, price, currency, isavailable, nbusage, nbpicturesshowroom
          FROM service_provider_pack WHERE isavailable = {isavailable}
          """
        ).on(
          'isavailable -> true
        ).as(packType *)
    }
  }

  /**
   * Retrieve the requested pack types given its ID
   * @return the pack type if found
   */
  def getPackType(id: Pk[Long]): Option[ProviderPackType] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, name, nbmonths, price, currency, isavailable, nbusage, nbpicturesshowroom
          FROM service_provider_pack WHERE id = {id}
          """
        ).on(
          'id -> id
        ).as(packType.singleOpt)
    }
  }

  /**
   * Create a new pack given the pack type ID, and some new purchase info.
   * This method if used just before any purchase is saved (add provider, renewal).
   * @param id the pack type ID
   * @return a pack with some purchase info
   */
  def getPackForNewPurchase(id: Pk[Long]): Option[ProviderPack] = {
    getPackType(id) match {
      case Some(packType) => {
        val since = new Date
        val until = Calendar.getInstance()
        until.setTime(since)
        until.add(Calendar.MONTH, packType.nbMonths)
        Some(ProviderPack(packType, ProviderPurchase(null, 0, id.get, since, until.getTime, packType.price, packType.currency, false, None, None, None, None)))
      }
      case None => None
    }
  }

  /**
   * Retrieve the PayPal transaction info for a given purchase
   * @param purchaseId the purchase ID
   * @return the transaction info if found
   */
  def getTransaction(purchaseId: Long): Option[ProviderTransaction] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT purchaseid, amount, currency, token, correlationid, payerid, transactionid, accountid
          FROM service_provider_transaction
          WHERE purchaseid = {purchaseid}
          """
        ).on(
          'purchaseid -> purchaseId
        ).as(transaction.singleOpt)
    }
  }

  /**
   * Insert the given transaction and update the payment status of the corresponding purchase
   * @param purchase the purchase to be updated (as paid)
   * @param transaction the transaction to be saved
   */
  def completePurchaseWithTransaction(purchase: ProviderPurchase, transaction: ProviderTransaction) = {
    DB.withTransaction {
      implicit connection =>
        SQL(
          """
          INSERT INTO service_provider_transaction (
            purchaseid, amount, currency, token, correlationid, payerid, transactionid, accountid
          ) VALUES (
            {purchaseid}, {amount}, {currency}, {token}, {correlationid}, {payerid}, {transactionid}, {accountid}
          )
          """
        ).on(
          'purchaseid -> transaction.purchaseId,
          'amount -> transaction.amount,
          'currency -> transaction.currency,
          'token -> transaction.token,
          'correlationid -> transaction.correlationId,
          'payerid -> transaction.payerId,
          'transactionid -> transaction.transactionId,
          'accountid -> transaction.accountId
        ).executeInsert()
        updatePurchasePaymentWithConnection(purchase)
    }
  }

  /**
   * Retrieve the full purchase info for a given purchase ID (i.e. provider, pack and optionally transaction)
   * @param purchaseId the purchase ID
   * @return a complete provider if found
   */
  def getProviderForPurchase(purchaseId: Pk[Long]): Option[Provider] = {
    getPurchase(purchaseId) match {
      case Some(purchase) => {
        getProvider(Id(purchase.providerId)) match {
          case Some(provider) => {
            getPackType(Id(purchase.packId)) match {
              case Some(packType) => {
                val transaction = models.providers.Provider.getTransaction(purchaseId.get)
                val providerPack  = ProviderPack(packType, purchase.copy(transaction = transaction))
                Some(provider.copy(currentPack = Some(providerPack)))
              }
              case None =>
                None
            }
          }
          case None =>
            None
        }
      }
      case None =>
        None
    }
  }

  /**
   * Retrieve the purchase info given its ID (and not provider ID)
   * @param purchaseId the purchase ID
   * @return the purchase if found
   */
  def getPurchase(purchaseId: Pk[Long]): Option[ProviderPurchase] = {
    DB.withConnection {
      implicit connection =>
        SQL(
        """
          SELECT id, providerid, packid, since, until, amount, currency, paid, paymentdate, paymentmethod, paymentreference
          FROM service_provider_purchase
          WHERE
            id = {id}
        """
        ).on(
          'id -> purchaseId
        ).as(purchase.singleOpt)
    }
  }

  /**
   * Find the stats of a provider given its ID
   * @param id the provider id
   * @return the stats
   */
  def getAllProviderStats(id: Pk[Long]): ProviderStats = {
    DB.withConnection {
      implicit connection =>
        val statsEvents = SQL(
          """
          SELECT count(date) as nb, providerid, eventtype
          FROM service_provider_stats
          WHERE providerid = {id}
          GROUP BY providerid, eventtype
          """
        ).on(
          'id -> id
        ).as(statsEventTotal *)

        ProviderStats(
          id,
          statsEvents.find(e => e.eventType == models.providers.Provider.STATS_EVENT_TYPE_DISPLAY),
          statsEvents.find(e => e.eventType == models.providers.Provider.STATS_EVENT_TYPE_DETAIL),
          statsEvents.find(e => e.eventType == models.providers.Provider.STATS_EVENT_TYPE_MAIL),
          statsEvents.find(e => e.eventType == models.providers.Provider.STATS_EVENT_TYPE_WEBSITE)
        )
    }
  }

  /**
   * Insert a new provider stats event (display, detail, mail or website)
   * @param event the event to be recorded
   */
  def insertProviderStatsEvent(event: ProviderStatsEvent) {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          INSERT INTO service_provider_stats (
            date, ipaddress, providerid, eventtype
          ) VALUES (
            {date}, {ipaddress}, {providerid}, {eventtype}
          )
          """
        ).on(
          'date -> event.date,
          'ipaddress -> event.IPAddress,
          'providerid -> event.providerId,
          'eventtype -> event.eventType
        ).executeInsert()
    }
  }

  /**
   * Find all the categories for providers
   * @return a list of categories
   */
  def getProviderCategories: List[ProviderCategory] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, name
          FROM service_provider_cat
          """
        ).as(category *)
    }
  }

  /**
   * Insert a new logo picture
   * @param logoPicture the logo info
   * @param file the logo file
   */
  def addLogo(logoPicture: ProviderLogoPicture, file: File) = {
    DB.withTransaction {
      implicit connection =>
        store.saveProviderLogoPicture(logoPicture, file)
        deleteLogoPicture(logoPicture.providerId)
        addLogoPicture(logoPicture)
        editProviderLogo(Id(logoPicture.providerId), true)
    }
  }

  /**
   * Delete the logo picture
   * @param logoPicture the picture info
   */
  def deleteLogo(logoPicture: ProviderLogoPicture) = {
    DB.withTransaction {
      implicit connection =>
        store.deleteProviderLogoPicture(logoPicture)
        deleteLogoPicture(logoPicture.providerId)
        editProviderLogo(Id(logoPicture.providerId), false)
    }
  }

  /**
   * Load the logo picture info for the given provider, if any
   * @return the logo picture info if found
   */
  def getLogo(providerId: Long): Option[ProviderLogoPicture] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT providerid, filename, contenttype
          FROM service_provider_logo WHERE providerid = {providerid}
          """
        ).on(
          'providerid -> providerId
        ).as(logo.singleOpt)
    }
  }

  /**
   * Return the pre signed logo picture link for direct download (but with expiration date)
   * @param logoPicture the logo picture to link
   * @return a string with the URL of the logo picture direct download
   */
  def getProviderLogoPictureLink(logoPicture: ProviderLogoPicture): Option[String] = {
    store.getProviderLogoPictureLink(logoPicture)
  }

  /**
   * Remove the logo picture info for given provider, if any
   * @param providerId the provider ID
   */
  protected def deleteLogoPicture(providerId: Long)(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM service_provider_logo
      WHERE
       providerid = {providerid}
      """
    ).on(
      'providerid -> providerId
    ).executeUpdate()
  }

  /**
   * Insert the logo picture info for given provider
   * @param logoPicture the logo info
   */
  protected def addLogoPicture(logoPicture: ProviderLogoPicture)(implicit connection: Connection) = {
    SQL(
      """
      INSERT INTO service_provider_logo (
        providerid, filename, contenttype
      ) VALUES (
        {providerid}, {filename}, {contenttype}
      )
      """
    ).on(
      'providerid -> logoPicture.providerId,
      'filename -> logoPicture.filename,
      'contenttype -> logoPicture.contentType
    ).executeInsert()
  }

  /**
   * Update the provider info for logo
   * @param providerId the provider ID
   * @param hasLogo is there a logo for this provider?
   */
  protected def editProviderLogo(providerId: Pk[Long], hasLogo: Boolean)(implicit connection: Connection) {
    SQL(
      """
      UPDATE service_provider
      SET
        haslogo = {haslogo}
      WHERE id = {id}
      """
    ).on(
      'id -> providerId,
      'haslogo -> hasLogo
    ).executeUpdate()
  }

  /**
   * Insert a new showroom picture
   * @param showroomPicture the picture info
   * @param file the picture file
   * @param thumb the thumbnail file
   */
  def addPicture(showroomPicture: ProviderShowroomPicture, file: File, thumb: File) = {
    DB.withTransaction {
      implicit connection =>
        val id = addShowroomPicture(showroomPicture)
        val pictureWithId = showroomPicture.copy(id = Id(id))
        store.saveProviderShowroomPicture(pictureWithId, file, thumb)
        pictureWithId
    }
  }

  /**
   * Delete the picture
   * @param showroomPicture the picture info
   */
  def deletePicture(showroomPicture: ProviderShowroomPicture) = {
    DB.withTransaction {
      implicit connection =>
        store.deleteProviderShowroomPicture(showroomPicture)
        deleteShowroomPicture(showroomPicture.id)
    }
  }

  /**
   * Load the pictures info for the given provider, if any
   * @return the logo pictures info if found
   */
  def getPictures(providerId: Long): List[ProviderShowroomPicture] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, providerid, filename, size, date, contenttype, title
          FROM service_provider_picture WHERE providerid = {providerid}
          """
        ).on(
          'providerid -> providerId
        ).as(picture *)
    }
  }

  /**
   * Return the pre signed picture link for direct download (but with expiration date)
   * @param showroomPicture the picture to link
   * @return a string with the URL of the picture direct download
   */
  def getProviderShowroomPictureLink(showroomPicture: ProviderShowroomPicture): Option[String] = {
    store.getProviderShowroomPictureLink(showroomPicture)
  }

  /**
   * Return the pre signed picture thumbnail link for direct download (but with expiration date)
   * @param showroomPicture the picture to link
   * @return a string with the URL of the picture thumbnail direct download
   */
  def getProviderShowroomPictureThumbnailLink(showroomPicture: ProviderShowroomPicture): Option[String] = {
    store.getProviderShowroomPictureThumbnailLink(showroomPicture)
  }

  /**
   * Remove the given picture info, if any
   * @param pictureId the picture ID
   */
  protected def deleteShowroomPicture(pictureId: Pk[Long])(implicit connection: Connection) = {
    SQL(
      """
      DELETE FROM service_provider_picture
      WHERE
       id = {id}
      """
    ).on(
      'id -> pictureId
    ).executeUpdate()
  }

  /**
   * Insert the picture info for given provider
   * @param showroomPicture the picture info
   */
  protected def addShowroomPicture(showroomPicture: ProviderShowroomPicture)(implicit connection: Connection): Long = {
    val id: Option[Long] = SQL(
      """
      INSERT INTO service_provider_picture (
        providerid, filename, size, date, contenttype, title
      ) VALUES (
        {providerid}, {filename}, {size}, {date}, {contenttype}, {title}
      )
      """
    ).on(
      'providerid -> showroomPicture.providerId,
      'filename -> showroomPicture.filename,
      'size -> showroomPicture.size,
      'date -> showroomPicture.date,
      'contenttype -> showroomPicture.contentType,
      'title -> showroomPicture.title
    ).executeInsert()
    id.get
  }
}
