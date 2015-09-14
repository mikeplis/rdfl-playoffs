package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import utils._

// TODO: unify "team" vs "franchise"
// TODO: write tests
// TODO: figure out if conf file should be used instead of environment variables
// TODO: fix all of the unsafe calls like toDouble and sys.env()

object Application extends Controller {

  val adminForm = Form(
    mapping(
      "advancers" -> number(min = 0, max = 100),
      "teamId" -> list(nonEmptyText)
    )(AdminData.apply)(AdminData.unapply)
  )

  def index = Action {
    val liveScores = formatLiveScoresForDisplay(MFLService.liveScores)
    val advancerCount = RedisService.getAdvancerCount
    Ok(views.html.index(liveScores, advancerCount))
  }

  def admin = Action {
    MFLService.franchises.fold(InternalServerError("error getting list of franchises")) { franchises =>
      val filledForm = adminForm.fill(
        AdminData(
          advancerCount = RedisService.getAdvancerCount,
          teamIds = RedisService.getTeamIds
      ))
      Ok(views.html.admin(filledForm, franchises))
    }
  }

  def adminPost = Action { implicit request =>
    adminForm.bindFromRequest.fold(
      formWithErrors => {
        val franchiseList = MFLService.franchises.getOrElse(List())
        BadRequest(views.html.admin(formWithErrors, franchiseList))
      },
      adminData => {
        RedisService.setAdvancerCount(adminData.advancerCount)
        RedisService.setTeamIds(adminData.teamIds)
        Redirect(routes.Application.index())
      }
    )
  }

  private def formatLiveScoresForDisplay(liveScoring: LiveScoring): Seq[LiveScoreForDisplay] = {
    val teamIds = RedisService.getTeamIds
    val idToNameMap = MFLService.franchises.fold(Map.empty[String, String])(_.map(f => f.id -> f.name).toMap)

    liveScoring.franchises.filter(franchise => teamIds.contains(franchise.id)).map { liveScore =>
      LiveScoreForDisplay(
        name = idToNameMap.getOrElse(liveScore.id, ""),
        score = liveScore.score,
        gameSecondsRemaining = liveScore.gameSecondsRemaining,
        playersYetToPlay = liveScore.playersYetToPlay,
        playersCurrentlyPlaying = liveScore.playersCurrentlyPlaying)
    }
  }
}

case class AdminData(advancerCount: Int, teamIds: List[String])
case class LiveScoreForDisplay(name: String, score: Double, gameSecondsRemaining: Int, playersYetToPlay: Int, playersCurrentlyPlaying: Int)