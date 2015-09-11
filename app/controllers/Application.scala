package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import com.redis._


import java.net.URI

import utils._

// TODO: unify "team" vs "franchise"
// TODO: use Reads to turn json into Scala objects
// TODO: add wrapper around redis client
// TODO: write tests
// TODO: move MFL data retrieval into object
// TODO: figure out if conf file should be used instead of environment variables
// TODO: fix all of the unsafe calls like toDouble and sys.env()

object Application extends Controller {

  lazy val redis = {
    val uri = new URI(sys.env("REDIS_URL"))
    val secret = uri.getUserInfo.split(":",2).lastOption
    new RedisClient(uri.getHost, uri.getPort, secret = secret)
  }

  lazy val adminForm = Form(
    mapping(
      "advancers" -> number(min = 0, max = 100),
      "teamId" -> list(nonEmptyText)
    )(AdminData.apply)(AdminData.unapply)
  )

  val TeamIds = "teamIds"
  val AdvancerCount = "advancerCount"

  def index = Action {
    val liveScores = formatLiveScoresForDisplay(MFLClient.liveScores)
    val advancerCount = redis.get(AdvancerCount).map(_.toInt).getOrElse(0)
    Ok(views.html.index(liveScores, advancerCount))
  }

  def test = Action {
    val liveScores = formatLiveScoresForDisplay(MockMFLClient.liveScores)
    val advancerCount = redis.get(AdvancerCount).map(_.toInt).getOrElse(0)
    Ok(views.html.index(liveScores, advancerCount))
  }

  def admin = Action {
    MFLClient.franchises.fold(InternalServerError("error getting list of franchises")) { franchises =>
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
        val franchiseList = MFLClient.franchises.getOrElse(List())
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

  def formatLiveScoresForDisplay(liveScores: Seq[LiveScore]): Seq[LiveScoreForDisplay] = {
    // get tracked team ids from redis
    val teamIds: List[String] = redis.lrange(TeamIds, 0, -1).fold(List.empty[String])(_.flatten)

    val idToName = MFLClient.franchises.fold(Map.empty[String, String])(_.map(f => f.id -> f.name).toMap)

    liveScores.filter(liveScore => teamIds.contains(liveScore.id)).map { liveScore => 
      LiveScoreForDisplay(
        name = idToName.getOrElse(liveScore.id, ""),
        score = liveScore.score,
        gameSecondsRemaining = liveScore.gameSecondsRemaining,
        playersYetToPlay = liveScore.playersYetToPlay,
        playersCurrentlyPlaying = liveScore.playersCurrentlyPlaying)
    }
  }
}

case class AdminData(advancerCount: Int, teamIds: List[String])
case class LiveScoreForDisplay(name: String, score: Double, gameSecondsRemaining: Int, playersYetToPlay: Int, playersCurrentlyPlaying: Int)