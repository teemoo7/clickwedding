package controllers.captcha

import scala.util.Random
import play.api.cache.Cache
import java.util.UUID
import play.api.Play.current
import models.authentication.User
import play.api.i18n.Lang
import play.api.templates.Html

/**
 * Created with IntelliJ IDEA.
 * User: Micael
 * Date: 18.12.12
 * Time: 08:29
 * To change this template use File | Settings | File Templates.
 */
trait TravelCaptcha extends Captcha {

  val CACHE_PREFIX = "travelCaptcha_"

  val COUNTRIES = Map(
    "CH" -> Seq("Matterhorn", "Federer"),
    "IT" -> Seq("Pizza", "Colosseo"),
    "FR" -> Seq("Eiffel tower", "Bordeaux red wine bottle"),
    "DE" -> Seq("Reichstag", "Oktoberfest beer"),
    "GB" -> Seq("UK Queen", "Big Ben"),
    "ES" -> Seq("Torero", "Paella"),
    "GR" -> Seq("Acropole", "Olive oil"),
    "US" -> Seq("Liberty statue", "Hollywood sign"),
    "CN" -> Seq("Great wall", "Forbidden city"),
    "JP" -> Seq("Fuji", "Japanese garden"),
    "PE" -> Seq("Machu Picchu", "Lama"),
    "BR" -> Seq("Rio de Janeiro statue", "Football Brazil team"),
    "MX" -> Seq("Sombrero", "Tacos"),
    "RU" -> Seq("Vodka", "Red square church")
  )

  def generateCaptcha: String = {
    val rand = new Random(System.currentTimeMillis());
    val random_index = rand.nextInt(COUNTRIES.size);
    val country = COUNTRIES.keySet.toArray.apply(random_index);
    val uuid = UUID.randomUUID().toString()
    Cache.set(CACHE_PREFIX.concat(uuid), country, 10*60)
    uuid
  }

  def validateCaptcha(value: String, uuid: String): Boolean = {
    val cacheValue = Cache.getAs[String](CACHE_PREFIX.concat(uuid))
    cacheValue match {
      case Some(country: String) => country.equals(value)
      case _ => false
    }
  }

  def getCaptcha(uuid: String, field: play.api.data.Field)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang): Html = {
    val cacheValue = Cache.getAs[String](CACHE_PREFIX.concat(uuid))
    cacheValue match {
      case Some(country: String) => {
        val words = COUNTRIES(country)
        views.html.captchas.travel.captcha(words, uuid)
      }
      case _ => Html.empty
    }
  }

  def getCaptchaHead(uuid: String)(implicit request: play.api.mvc.RequestHeader, user: Option[User], lang: Lang): Html = {
    views.html.captchas.travel.head.render(request, user, lang)
  }

}