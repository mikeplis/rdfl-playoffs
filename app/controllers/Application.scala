package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import utils._

import scala.collection.immutable.HashSet

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

  def formatLiveScoresForDisplay(liveScoring: LiveScoring): Seq[LiveScoreForDisplay] = {
    val teamIds = RedisService.getTeamIds
    val idToNameMap = MFLService.franchises.fold(Map.empty[String, String])(_.map(f => f.id -> f.name).toMap)

    val activeFranchises = liveScoring.franchises.filter(franchise => teamIds.contains(franchise.id))

    val franchiseToProjectedScore = projectedToAdvance(activeFranchises)
    val franchisesProjectedToAdvance = {
      val orderedByProjection = franchiseToProjectedScore.toSeq.sortBy(- _._2)
      HashSet(orderedByProjection.take(RedisService.getAdvancerCount).map(_._1): _*)
    }

    activeFranchises.map { liveScore =>
      LiveScoreForDisplay(
        name = idToNameMap.getOrElse(liveScore.id, ""),
        score = liveScore.score,
        gameSecondsRemaining = liveScore.gameSecondsRemaining,
        playersYetToPlay = liveScore.playersYetToPlay,
        playersCurrentlyPlaying = liveScore.playersCurrentlyPlaying,
        projectedScore = franchiseToProjectedScore(liveScore.id),
        projectedToAdvance = franchisesProjectedToAdvance.contains(liveScore.id))
    }
  }

  def projectedToAdvance(franchises: Seq[LiveScoreFranchise]) = {
    val totalSecondsInGame = 3600
    val projectedScores = MFLService.projectedScores.scores.map(ps => ps.id -> ps.score).toMap
    franchises.map { franchise =>
      val liveFranchiseProjection = franchise.starters.foldLeft(0d){ case (sum, player) =>
        val livePlayerProjection = {
          val projectedScore = projectedScores.getOrElse(player.playerId, 0d)
          player.score + (player.gameSecondsRemaining / totalSecondsInGame) * projectedScore
        }
        sum + livePlayerProjection
      }
      franchise.id -> liveFranchiseProjection
    }.toMap
  }
}

case class AdminData(advancerCount: Int, teamIds: List[String])
case class LiveScoreForDisplay(name: String,
                               score: Double,
                               gameSecondsRemaining: Int,
                               playersYetToPlay: Int,
                               playersCurrentlyPlaying: Int,
                               projectedScore: Double,
                               projectedToAdvance: Boolean)