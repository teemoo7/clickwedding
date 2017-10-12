package controllers.captcha

import scala.util.Random
import play.api.cache.Cache
import java.util.UUID
import play.api.Play.current
import models.authentication.User
import play.api.i18n.Lang
import play.api.templates.Html
import play.api.Logger

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.12.12
 * Time: 08:29
 * To change this template use File | Settings | File Templates.
 */
trait TicTacToeCaptcha extends Captcha {

  val CACHE_PREFIX = "ticTacToeCaptcha_"

  val GAMES = List(
    Game(Seq(0, 1), Seq(3, 5), 2),
    Game(Seq(0, 6), Seq(4, 5), 3),
    Game(Seq(3, 4), Seq(6, 1), 5),
    Game(Seq(2, 8), Seq(4, 6), 5),
    Game(Seq(4, 7), Seq(0, 5), 1),
    Game(Seq(4, 5), Seq(6, 8), 3),
    Game(Seq(1, 4), Seq(0, 2), 7),
    Game(Seq(4, 8), Seq(3, 7), 0),
    Game(Seq(0, 6), Seq(1, 7), 3),
    Game(Seq(2, 8), Seq(3, 4), 5),
    Game(Seq(2, 5), Seq(4, 7), 8),
    Game(Seq(6, 7), Seq(0, 4), 8),
    Game(Seq(4, 8), Seq(2, 6), 0),
    Game(Seq(4, 7), Seq(2, 3), 1),
    Game(Seq(0, 4), Seq(3, 5), 8)
  )

  val WINING_ROWS = List(
    Seq(0, 1, 2),
    Seq(3, 4, 5),
    Seq(6, 7, 8),
    Seq(0, 3, 6),
    Seq(1, 4, 7),
    Seq(2, 5, 8),
    Seq(0, 4, 8),
    Seq(2, 4, 6)
  )

  case class Game(o: Seq[Int], x: Seq[Int], win: Int)

  def generateCaptcha: String = {
    val rand = new Random(System.currentTimeMillis())
    val random_index = rand.nextInt(GAMES.size)
    val game = GAMES(random_index)
    val uuid = UUID.randomUUID().toString()
    Cache.set(CACHE_PREFIX.concat(uuid), game, 10*60)
    uuid
  }

  def validateCaptcha(value: String, uuid: String): Boolean = {
    val cacheGame = Cache.getAs[Game](CACHE_PREFIX.concat(uuid))
    cacheGame match {
      case Some(game: Game) =>
        game.win == value.toInt
      case _ =>
        false
    }
  }

  def getCaptcha(uuid: String, field: play.api.data.Field)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang): Html = {
    val cacheGame = Cache.getAs[Game](CACHE_PREFIX.concat(uuid))
    cacheGame match {
      case Some(game: Game) => {
        views.html.captchas.ticTacToe.captcha(uuid, field)
      }
      case _ => Html.empty
    }
  }

  def getCaptchaHead(uuid: String)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang): Html = {
    val cacheGame = Cache.getAs[Game](CACHE_PREFIX.concat(uuid))
    cacheGame match {
      case Some(game: Game) => {
        views.html.captchas.ticTacToe.head.render(game.o, game.x, request, user, lang)
      }
      case _ => Html.empty
    }
  }

  /**
   * Testcase to verify that each game is possible
   */
  def checkGames() {
    GAMES.map( game => {
      val cells = game.o ++ game.x :+ game.win
      cells.map ( cell => {
        if(cells.filter(c => c == cell).length == 1) {
          Logger.info("[OK] Game correctly initialized!")
        } else {
          Logger.error("[ERR] Game with wrong initial positions! "+game.toString)
        }
      })
      val winning = (game.o :+ game.win).sorted
      val winningCase = WINING_ROWS.filter(w => w.containsSlice(winning))
      if (winningCase.length == 1) {
        Logger.info("[OK] Game can be won! "+game.toString + " " + winningCase.mkString(", "))
      } else {
        Logger.error("[ERR] Game cannot be won! "+game.toString)
      }
    })
  }
}