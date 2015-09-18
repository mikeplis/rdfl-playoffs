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

  val redis = new RedisService

  val adminForm = Form(
    mapping(
      "advancers" -> number(min = 0, max = 100),
      "teamId" -> list(nonEmptyText)
    )(AdminData.apply)(AdminData.unapply)
  )

  def index = Action {
    val (liveResults, overviewStats) = getLiveResults(MFLService.liveScores)
    val advancerCount = redis.getAdvancerCount
    Ok(views.html.index(liveResults, overviewStats, advancerCount))
  }

  def admin = Action {
    MFLService.franchises.fold(InternalServerError("error getting list of franchises")) { franchises =>
      val filledForm = adminForm.fill(
        AdminData(
          advancerCount = redis.getAdvancerCount,
          teamIds = redis.getTeamIds
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
        redis.setAdvancerCount(adminData.advancerCount)
        redis.setTeamIds(adminData.teamIds)
        Redirect(routes.Application.index())
      }
    )
  }

  def getLiveResults(liveScoring: LiveScoring) = {
    val teamIds = redis.getTeamIds
    val idToNameMap = MFLService.franchises.fold(Map.empty[String, String])(_.map(f => f.id -> f.name).toMap)

    val activeFranchises = liveScoring.franchises.filter(franchise => teamIds.contains(franchise.id))

    val franchiseToProjectedScore = liveProjections(activeFranchises)

    val franchisesProjectedToAdvance: HashSet[String] = {
      // sort franchises by their live projected scores
      val orderedByProjection = franchiseToProjectedScore.toSeq.sortBy(- _._2)
      // create a Set of the top N franchises by projected score, where N in the number of advancers set by the admin
      val projectedToAdvance = orderedByProjection.take(redis.getAdvancerCount).map(_._1)
      HashSet(projectedToAdvance: _*)
    }

    val overviewStats = {
      val sizeOfLastInFirstOut = 4
      val (in, out) = franchiseToProjectedScore.toSeq.sortBy(- _._2).splitAt(redis.getAdvancerCount)
      OverviewStats(
        projectedCut = in.lastOption.fold(0d)(_._2),
        lastIn = in.takeRight(sizeOfLastInFirstOut).map(x => idToNameMap.getOrElse(x._1, "")),
        firstOut = out.take(sizeOfLastInFirstOut).map(x => idToNameMap.getOrElse(x._1, "")))
    }

    val liveResults = activeFranchises.map { liveScore =>
      LiveResultRow(
        name = idToNameMap.getOrElse(liveScore.id, ""),
        score = liveScore.score,
        gameSecondsRemaining = liveScore.gameSecondsRemaining,
        playersYetToPlay = liveScore.playersYetToPlay,
        playersCurrentlyPlaying = liveScore.playersCurrentlyPlaying,
        projectedScore = franchiseToProjectedScore(liveScore.id),
        projectedToAdvance = franchisesProjectedToAdvance.contains(liveScore.id))
    }
    (liveResults, overviewStats)
  }

  /** Calculate the live projected score for each franchise
   *
   * @param franchises - list of franchises to calculate projections for
   * @return - map of franchise ID to live projected score
   */
  def liveProjections(franchises: Seq[LiveScoreFranchise]): Map[String, Double] = {
    // a football game is 60 minutes in length
    // type is Double so that integer division is not used when calculating live player projections
    val totalSecondsInGame = 3600d
    // retrieve pre-game projected scores for all players
    val pregameProjections = MFLService.projectedScores.scores.map(ps => ps.id -> ps.score).toMap
    franchises.map { franchise =>
      // sum up the live projected scores for each player
      val liveFranchiseProjection = franchise.starters.foldLeft(0d){ case (sum, player) =>
        // calculate live projected score for each player
        val livePlayerProjection = {
          // retrieve pre-game projected score for player
          val pregameProjection = pregameProjections.getOrElse(player.id, 0d)
          // add player's current score to his pre-game projected score, with the pre-game projected score weighted by the
          // number of seconds remaining in the game, i.e. as the game moves on, a player's live projected score will converge
          // toward his current score
          player.score + (player.gameSecondsRemaining / totalSecondsInGame) * pregameProjection
        }
        sum + livePlayerProjection
      }
      franchise.id -> liveFranchiseProjection
    }.toMap
  }
}

case class AdminData(advancerCount: Int, teamIds: List[String])
case class LiveResults(results: Seq[LiveResultRow],
                       overviewStats: OverviewStats)
case class LiveResultRow(name: String,
                         score: Double,
                         gameSecondsRemaining: Int,
                         playersYetToPlay: Int,
                         playersCurrentlyPlaying: Int,
                         projectedScore: Double,
                         projectedToAdvance: Boolean)
case class OverviewStats(projectedCut: Double,
                         lastIn: Seq[String],
                         firstOut: Seq[String])