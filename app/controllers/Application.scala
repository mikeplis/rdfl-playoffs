package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import play.api.libs.json._
import scala.io.Source

import com.redis._

import scala.util.Random

// TODO: unify "team" vs "franchise"
// TODO: use Reads to turn json into Scala objects

object Application extends Controller {

  lazy val redis = new RedisClient("localhost", 6379)

  lazy val adminForm = Form(
    mapping(
      "advancers" -> number(min = 0, max = 100),
      "teamId" -> list(nonEmptyText)
    )(AdminData.apply)(AdminData.unapply)
  )

  val TeamIds = "teamIds"
  val AdvancerCount = "advancerCount"

  def index = Action {
    val liveScores = getLiveScoresForDisplay()
    val advancerCount = redis.get(AdvancerCount).map(_.toInt).getOrElse(0)
    Ok(views.html.index(liveScores, advancerCount))
  }

  def test = Action {
    val liveScores = getLiveScoresForDisplay(debug = true)
    val advancerCount = redis.get(AdvancerCount).map(_.toInt).getOrElse(0)
    Ok(views.html.index(liveScores, advancerCount))
  }

  def admin = Action {
    getFranchiseListFromMFL.fold(InternalServerError("error getting list of franchises")) { franchises =>
      val filledForm = adminForm.fill(
        AdminData(
          advancerCount = redis.get(AdvancerCount).map(_.toInt).getOrElse(0),
          teamIds = redis.lrange(TeamIds, 0, -1).fold(List.empty[String])(_.flatten)
      ))
      Ok(views.html.admin(filledForm, franchises))
    }
  }

  def adminPost = Action { implicit request =>
    adminForm.bindFromRequest.fold(
      formWithErrors => {
        val franchiseList = getFranchiseListFromMFL.getOrElse(List())
        BadRequest(views.html.admin(formWithErrors, franchiseList))
      },
      adminData => {
        redis.set(AdvancerCount, adminData.advancerCount)
        // clear teamIds
        redis.ltrim(TeamIds, 1, 0)
        // add new ids to teamIds
        adminData.teamIds.foreach { teamId => 
          redis.lpush(TeamIds, teamId)
        }
        Redirect(routes.Application.index())
      }
    )
  }

  // get live scores for franchises stored in redis
  def getLiveScoresForDisplay(debug: Boolean = false): Seq[LiveScoreForDisplay] = {
    // get tracked team ids from redis
    val teamIds: List[String] = redis.lrange(TeamIds, 0, -1).fold(List.empty[String])(_.flatten)
    // get live scores from MFL
    val liveScores = getLiveScores(debug)

    val idToName = getFranchiseListFromMFL.fold(Map.empty[String, String])(_.map(f => f.id -> f.name).toMap)


    liveScores.filter(liveScore => teamIds.contains(liveScore.id)).map { liveScore => 
      LiveScoreForDisplay(
        name = idToName.getOrElse(liveScore.id, ""),
        score = liveScore.score,
        gameSecondsRemaining = liveScore.gameSecondsRemaining,
        playersYetToPlay = liveScore.playersYetToPlay,
        playersCurrentlyPlaying = liveScore.playersCurrentlyPlaying)
    }
  }

  def getLiveScores(debug: Boolean = false) = {
    val liveScoringUrl = "http://football26.myfantasyleague.com/2015/export?TYPE=liveScoring&L=34348&JSON=1"
    val json = Json.parse(Source.fromURL(liveScoringUrl).mkString)
    val matchups = json \\ "franchise"
    val liveScores = matchups.map(_.as[List[JsValue]]).flatten
    liveScores.map { liveScore =>
      val id = (liveScore \ "id").as[String]
      val score = if (debug) {
        Random.nextDouble * 200
      }
      else {
        (liveScore \ "score").as[String].toDouble
      }
      val gameSecondsRemaining = (liveScore \ "gameSecondsRemaining").as[String].toInt
      val playersYetToPlay = (liveScore \ "playersYetToPlay").as[String].toInt
      val playersCurrentlyPlaying = (liveScore \ "playersCurrentlyPlaying").as[String].toInt
      LiveScore(id, score, gameSecondsRemaining, playersYetToPlay, playersCurrentlyPlaying)
    }
  }

  def getFranchiseListFromMFL: Option[List[Franchise]] = {
    val league = {
      // TODO: read this from config file
      val url = "http://football26.myfantasyleague.com/2015/export?TYPE=league&L=34348&W=&JSON=1"
      val source = Source.fromURL(url).mkString
      Json.parse(source)
    }
    val maybeFranchises = (league \\ "franchise").headOption.map(_.as[List[JsObject]])
    maybeFranchises.map(_.map { franchiseJson =>
      val name = (franchiseJson \ "name").as[String]
      val id = (franchiseJson \ "id").as[String]
      Franchise(name, id)
    })
  }
}

case class AdminData(advancerCount: Int, teamIds: List[String])
case class Franchise(name: String, id: String)
case class LiveScore(id: String, score: Double, gameSecondsRemaining: Int, playersYetToPlay: Int, playersCurrentlyPlaying: Int)
case class LiveScoreForDisplay(name: String, score: Double, gameSecondsRemaining: Int, playersYetToPlay: Int, playersCurrentlyPlaying: Int)