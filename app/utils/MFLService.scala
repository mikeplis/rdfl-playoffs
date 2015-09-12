package utils

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.io.Source
import scala.util.Random

trait MFLService {

  def franchises: Option[List[Franchise]] = {
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

  def liveScores: LiveScoring
}

object LiveMFLService extends MFLService {
  def liveScores = {
    val liveScoringUrl = "http://football26.myfantasyleague.com/2015/export?TYPE=liveScoring&L=34348&DETAILS=1&JSON=1"
    val json = Json.parse(Source.fromURL(liveScoringUrl).mkString)
    json.as[LiveScoring]
  }
}

object MockMFLService extends MFLService {
  def liveScores = {
    ???
//    val startingLineupSize = 14
//    franchises.fold(Seq.empty[LiveScore2])(_.map { franchise =>
//      val id = franchise.id
//      val playersCurrentlyPlaying = 0
//      val playersYetToPlay = Random.nextInt(startingLineupSize + 1)
//      val gameSecondsRemaining = 3600 * playersYetToPlay
//      val score = {
//        val playersPlayed = startingLineupSize - playersYetToPlay
//        (1 to playersPlayed).map { _ =>
//          // generate random double from 5 to 15 to simulate each player's score
//          (Random.nextDouble * 10) + 5
//        }.sum
//      }
//      LiveScore2(id, score, gameSecondsRemaining, playersYetToPlay, playersCurrentlyPlaying)
//    })
  }
}

case class Franchise(name: String, id: String)
case class LiveScore2(id: String, score: Double, gameSecondsRemaining: Int, playersYetToPlay: Int, playersCurrentlyPlaying: Int)

case class LiveScorePlayer(playerId: String, score: Double, gameSecondsRemaining: Int, status: String)
object LiveScorePlayer {
  implicit val reads: Reads[LiveScorePlayer] = (
    (__ \ "id").read[String] and
    (__ \ "score").read[String].map(_.toDouble) and
    (__ \ "gameSecondsRemaining").read[String].map(_.toInt) and
    (__ \ "status").read[String]
  )(LiveScorePlayer.apply _)
}

case class LiveScorePlayers(players: Seq[LiveScorePlayer])
object LiveScorePlayers {
  // below code was not working, see here: http://stackoverflow.com/questions/15042205/how-to-serialize-deserialize-case-classes-to-from-json-in-play-2-1
//  implicit val reads: Reads[LiveScorePlayers] = (
//    (__ \ "player").read[Seq[LiveScorePlayer]]
//  )(LiveScorePlayers.apply _)
  implicit val reads: Reads[LiveScorePlayers] = (__ \ "player").read[Seq[LiveScorePlayer]].map(LiveScorePlayers.apply)
}

case class LiveScoreFranchise(franchiseId: String,
                              score: Double,
                              gameSecondsRemaining: Int,
                              playersYetToPlay: Int,
                              playersCurrentlyPlaying: Int,
                              players: Seq[LiveScorePlayer])
object LiveScoreFranchise {
  implicit val reads: Reads[LiveScoreFranchise] = (
    (__ \ "id").read[String] and
    (__ \ "score").read[String].map(_.toDouble) and
    (__ \ "gameSecondsRemaining").read[String].map(_.toInt) and
    (__ \ "playersYetToPlay").read[String].map(_.toInt) and
    (__ \ "playersCurrentlyPlaying").read[String].map(_.toInt) and
    (__ \ "players" \ "player").read[Seq[LiveScorePlayer]]
  )(LiveScoreFranchise.apply _)
}

case class LiveScoreMatchup(franchises: Seq[LiveScoreFranchise])
object LiveScoreMatchup {
  implicit val reads: Reads[LiveScoreMatchup] = (__ \ "franchise").read[Seq[LiveScoreFranchise]].map(LiveScoreMatchup.apply)
}

case class LiveScoring(matchups: Seq[LiveScoreMatchup])
object LiveScoring {
  implicit val reads: Reads[LiveScoring] = (__ \ "liveScoring" \ "matchup").read[Seq[LiveScoreMatchup]].map(LiveScoring.apply)
}
