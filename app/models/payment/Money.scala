package models.payment

import anorm._
import java.util.Date
import anorm.SqlParser._
import play.api.db.DB
import anorm.~
import models.wedding.Wedding
import play.api.Play.current
import models.modules.{LazyModule, Module}
import models.authentication.User
import java.sql.Connection

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 07.01.13
 * Time: 16:55
 * To change this template use File | Settings | File Templates.
 */

case class Transaction(id: Pk[Long] = NotAssigned, date: Date, weddingId: Long, user: User, amount: Int, balance: Int, action: TransactionType)

abstract class TransactionType

case class Payment(id: Pk[Long] = NotAssigned, moneyAmount: Int, totalCost: Double, currency: String, token: Option[String], correlationId: Option[String], payerId: Option[String], transactionId: Option[String], accountId: Option[String], weddingUid: String, weddingId: Long) extends TransactionType {
  override def toString() = {
    "Payment("+ (Seq(id, moneyAmount, totalCost, currency, token, correlationId, payerId, transactionId, accountId, weddingUid, weddingId) mkString(", ")) + ")"
  }
}
case class Purchase(module: LazyModule) extends TransactionType
case class Promotion(id: Pk[Long] = NotAssigned, code: String, amount: Int, remaining: Option[Int], deadline: Option[Date], weddingId: Option[Long]) extends TransactionType {
  // Check if the promotion is still valid
  def isValid: Boolean = {
    if (this.deadline.isDefined && this.deadline.get.before(new Date())) {
      return false
    }
    if (this.remaining.isDefined && this.remaining.get < 1) {
      return false
    }
    return true
  }
}
case class Opening() extends TransactionType

object Money {

  val transaction =  {
    get[Pk[Long]]("id") ~
      get[Long]("weddingid") ~
      get[Date]("date") ~
      get[Long]("userid") ~
      get[Int]("amount") ~
      get[Int]("balance") ~
      get[Int]("action") ~
      get[Long]("reference") map {
      case id ~ weddingId ~ date ~ userId ~ amount ~ balance ~ action ~ reference => Transaction(id, date, weddingId, models.authentication.User.findById(userId).get, amount, balance, getTransactionType(action, reference))
    }
  }

  val payment = {
    get[Pk[Long]]("id") ~
    get[Long]("weddingid") ~
    get[Int]("moneyamount") ~
    get[Double]("totalcost") ~
    get[String]("currency") ~
    get[Option[String]]("token") ~
    get[Option[String]]("correlationid") ~
    get[Option[String]]("payerid") ~
    get[Option[String]]("transactionid") ~
    get[Option[String]]("accountid") map {
      case id ~ weddingId ~ moneyAmount ~ totalCost ~ currency ~ token ~ correlationId ~ payerId ~ transactionId ~ accountId => Payment(id, moneyAmount, totalCost, currency, token, correlationId, payerId, transactionId, accountId, Wedding.findById(weddingId).get.uid, weddingId)
    }
  }

  val promotion = {
    get[Pk[Long]]("id") ~
    get[String]("code") ~
    get[Int]("amount") ~
    get[Option[Int]]("remaining") ~
    get[Option[Date]]("deadline") ~
    get[Option[Long]]("weddingid") map {
      case id ~ code ~ amount ~ remaining ~ deadline ~ weddingId => Promotion(id, code, amount, remaining, deadline, weddingId)
    }
  }

  /**
   * Create an transaction type object that represents the transaction action (payment, purchase, promotion) with its details
   * @param action
   * @param reference
   * @return the transaction object
   */
  def getTransactionType(action: Int, reference: Long): TransactionType = {
    action match {
      case controllers.payment.Money.ACTION_OPENING => {
        Opening()
      }
      case controllers.payment.Money.ACTION_PAYMENT => {
        getPayment(reference).get
      }
      case controllers.payment.Money.ACTION_PURCHASE => {
        getPurchase(reference).get
      }
      case controllers.payment.Money.ACTION_PROMOTION => {
        getPromotion(reference).get
      }
    }
  }

  /**
   * Find a transaction given its ID
   * @param id
   * @return the transaction if found
   */
  def getTransaction(id: Long): Option[Transaction] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id,weddingid, date, userid, amount, balance, action, reference FROM money_transaction WHERE id = {id}
          """
        ).on(
          'id -> id
        ).as(transaction.singleOpt)
    }
  }

  /**
   * Find a payment given its ID
   * @param id the payment id
   * @return the payment if found
   */
  def getPayment(id: Long): Option[Payment] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, weddingid, moneyamount, totalcost, currency, token, correlationid, payerid, transactionid, accountid FROM money_payment WHERE id = {id}
          """
        ).on(
          'id -> id
        ).as(payment.singleOpt)
    }
  }

  /**
   * Find a purchase (i.e. module) given its ID
   * @param id the module id
   * @return the purchase if found
   */
  def getPurchase(id: Long): Option[Purchase] = {
    Module.getModule(id) match {
      case Some(module) => Some(Purchase(module))
      case None => None
    }
  }

  /**
   * Find a promotion code details given its ID
   * @param id the promotion code id
   * @return the promotion if found
   */
  def getPromotion(id: Long): Option[Promotion] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, code, amount, remaining, deadline, weddingid FROM money_promotion WHERE id = {id}
          """
        ).on(
          'id -> id
        ).as(promotion.singleOpt)
    }
  }

  /**
   * Find a promotion code details given its code
   * @param code the code of the promotion
   * @return the promotion if found
   */
  def getPromotionByCode(code: String): Option[Promotion] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, code, amount, remaining, deadline, weddingid FROM money_promotion WHERE code = {code}
          """
        ).on(
          'code -> code
        ).as(promotion.singleOpt)
    }
  }

  /**
   * Insert a new payment transaction and update the wedding balance
   * @param wedding
   * @param user
   * @param payment
   * @return the transaction ID
   */
  def addPayment(wedding: Wedding, user: User, payment: Payment) = {
    DB.withTransaction {
      implicit connection =>
        val newBalance = getCurrentBalance(wedding.id) + payment.moneyAmount
        val id: Option[Long] = SQL(
          """
          INSERT INTO money_payment (
            weddingid, moneyamount, totalcost, currency, token, correlationid, payerid, transactionid, accountid
          ) VALUES (
            {weddingid}, {moneyamount}, {totalcost}, {currency}, {token}, {correlationid}, {payerid}, {transactionid}, {accountid}
          )
          """
        ).on(
          'weddingid -> wedding.id,
          'moneyamount -> payment.moneyAmount,
          'totalcost -> payment.totalCost,
          'currency -> payment.currency,
          'token -> payment.token,
          'correlationid -> payment.correlationId,
          'payerid -> payment.payerId,
          'transactionid -> payment.transactionId,
          'accountid -> payment.accountId
        ).executeInsert()
        val paymentId = id.get
        val transactionId = addTransaction(wedding.id, user.userId, payment.moneyAmount, newBalance, controllers.payment.Money.ACTION_PAYMENT, paymentId)
        transactionId
    }
  }

  /**
   * Insert a new purchase transaction and update the wedding balance
   * @param wedding
   * @param user
   * @param purchase
   * @return the transaction ID
   */
  def addPurchase(wedding: Wedding, user: User, purchase: Purchase)(implicit connection: Connection) = {
    val newBalance = getCurrentBalance(wedding.id) - purchase.module.price
    addTransaction(wedding.id, user.userId, purchase.module.price, newBalance, controllers.payment.Money.ACTION_PURCHASE, purchase.module.id.get)
  }

  /**
   * Insert a new promotion code transaction and update the wedding balance
   * @param wedding
   * @param user
   * @param promotion
   * @return the transaction ID
   */
  def addPromotion(wedding: Wedding, user: User, promotion: Promotion) = {
    DB.withTransaction {
      implicit connection =>
        val newBalance = getCurrentBalance(wedding.id) + promotion.amount
        if (promotion.remaining.isDefined) {
          SQL(
            """
            UPDATE money_promotion SET remaining = remaining-1 WHERE id = {id}
            """
          ).on(
            'id -> promotion.id
          ).executeUpdate()
        }
        val transactionId = addTransaction(wedding.id, user.userId, promotion.amount, newBalance, controllers.payment.Money.ACTION_PROMOTION, promotion.id.get)
        transactionId
    }
  }

  /**
   * Insert a new wedding creation (opening) transaction and set the wedding balance
   * @param wedding
   * @param user
   * @return the transaction ID
   */
  def addOpening(wedding: Wedding, user: User)(implicit connection: Connection) = {
    val amount = controllers.payment.Money.OPENING_BONUS_AMOUNT
    val transactionId = addTransaction(wedding.id, user.userId, amount, amount, controllers.payment.Money.ACTION_OPENING, 0)
    transactionId
  }

  /**
   * Insert a new transaction and update the wedding balance
   * @param weddingId
   * @param userId
   * @param moneyAmount
   * @param balance
   * @param action
   * @param reference
   * @return the transaction ID
   */
  def addTransaction(weddingId: Pk[Long], userId: Pk[Long], moneyAmount: Int, balance: Int, action: Int, reference: Long)(implicit connection: Connection) = {
    updateWeddingBalance(weddingId, balance)
    val id: Option[Long] = SQL(
      """
      INSERT INTO money_transaction (
        weddingid, date, userid, amount, balance, action, reference
      ) VALUES (
        {weddingid}, NOW(), {userid}, {amount}, {balance}, {action}, {reference}
      )
      """
    ).on(
      'weddingid -> weddingId,
      'userid -> userId,
      'amount -> moneyAmount,
      'balance -> balance,
      'action -> action,
      'reference -> reference
    ).executeInsert()
    id.get
  }

  /**
   * Update the wedding balance (money)
   * @param weddingId the wedding ID
   * @param newBalance the remaining money
   */
  def updateWeddingBalance(weddingId: Pk[Long], newBalance: Int)(implicit connection: Connection) {
    SQL(
      """
      UPDATE wedding
      SET money = {money}
      WHERE id = {weddingId}
      """
    ).on(
      'money -> newBalance,
      'weddingId -> weddingId
    ).executeUpdate()
  }

  /**
   * Find all transactions for a wedding
   * @param weddingId the ID of the wedding
   * @return a list of the transactions
   */
  def getAllTransactions(weddingId: Pk[Long]): List[Transaction] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id,weddingid, date, userid, amount, balance, action, reference
          FROM money_transaction
          WHERE weddingid = {weddingid}
          ORDER BY date desc, id desc
          """
        ).on(
          'weddingid -> weddingId
        ).as(transaction*)
    }
  }

  /**
   * Get the current balance of a wedding
   * @param weddingId
   * @return the money balance
   */
  protected def getCurrentBalance(weddingId: Pk[Long])(implicit connection: Connection) = {
    SQL(
      """
      SELECT money FROM wedding WHERE id = {id}
      """
    ).on(
      'id -> weddingId
    ).as(get[Int]("money").single)
  }

  /**
   * Delete all transactions for the given wedding
   * @param weddingId the wedding ID
   * @param connection the transaction connection
   * @return 0 if nothing was deleted
   */
  def deleteTransactionsForWedding(weddingId: Pk[Long])(implicit connection: Connection) = {
    // Payments
    SQL(
      """
      DELETE FROM money_payment
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
    // Transactions
    SQL(
      """
      DELETE FROM money_transaction
      WHERE
       weddingid = {weddingId}
      """
    ).on(
      'weddingId -> weddingId
    ).executeUpdate()
  }

  /**
   * Retrive all promotion codes (even is not valid anymore)
   */
  def getAllPromotion: List[Promotion] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          SELECT id, code, amount, remaining, deadline, weddingid FROM money_promotion
          """
        ).as(promotion*)
    }
  }

  /**
   * Create a new promotion code given its attributes
   * @param promotion the new promotion code
   */
  def addNewPromotion(promotion: Promotion) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          INSERT INTO money_promotion (
            code, amount, remaining, deadline, weddingid
          ) VALUES (
            {code}, {amount}, {remaining}, {deadline}, {weddingid}
          )
          """
        ).on(
          'code -> promotion.code,
          'amount -> promotion.amount,
          'remaining -> promotion.remaining,
          'deadline -> promotion.deadline,
          'weddingid -> promotion.weddingId
        ).executeUpdate()
    }
  }

  /**
   * Update the promotion code given its attributes
   * @param promotion the promotion code
   */
  def updatePromotion(promotion: Promotion) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
          UPDATE money_promotion SET
            code = {code}, amount = {amount}, remaining = {remaining}, deadline = {deadline}, weddingid = {weddingid}
          WHERE id = {id}
          """
        ).on(
          'code -> promotion.code,
          'amount -> promotion.amount,
          'remaining -> promotion.remaining,
          'deadline -> promotion.deadline,
          'weddingid -> promotion.weddingId,
          'id -> promotion.id
        ).executeUpdate()
    }
  }
}
