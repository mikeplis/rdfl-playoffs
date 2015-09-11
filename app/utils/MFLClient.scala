package utils

import play.api.libs.json.{JsValue, JsObject, Json}

import scala.io.Source
import scala.util.Random


trait MFLClientT {

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

  def liveScores: Seq[LiveScore]
}

object MFLClient extends MFLClientT {
  def liveScores = {
    val liveScoringUrl = "http://football26.myfantasyleague.com/2015/export?TYPE=liveScoring&L=34348&JSON=1"
    val json = Json.parse(Source.fromURL(liveScoringUrl).mkString)
    val matchups = json \\ "franchise"
    val liveScores = matchups.map(_.as[List[JsValue]]).flatten
    liveScores.map { liveScore =>
      val id = (liveScore \ "id").as[String]
      val score = (liveScore \ "score").as[String].toDouble
      val gameSecondsRemaining = (liveScore \ "gameSecondsRemaining").as[String].toInt
      val playersYetToPlay = (liveScore \ "playersYetToPlay").as[String].toInt
      val playersCurrentlyPlaying = (liveScore \ "playersCurrentlyPlaying").as[String].toInt
      LiveScore(id, score, gameSecondsRemaining, playersYetToPlay, playersCurrentlyPlaying)
    }
  }
}

object MockMFLClient extends MFLClientT {
  def liveScores: Seq[LiveScore] = {
    val startingLineupSize = 14
    franchises.fold(Seq.empty[LiveScore])(_.map { franchise =>
      val id = franchise.id
      val playersCurrentlyPlaying = 0
      val playersYetToPlay = Random.nextInt(startingLineupSize + 1)
      val gameSecondsRemaining = 3600 * playersYetToPlay
      val score = {
        val playersPlayed = startingLineupSize - playersYetToPlay
        (1 to playersPlayed).map { _ =>
          // generate random double from 5 to 15 to simulate each player's score
          (Random.nextDouble * 10) + 5
        }.sum
      }
      LiveScore(id, score, gameSecondsRemaining, playersYetToPlay, playersCurrentlyPlaying)
    })
  }
}

case class Franchise(name: String, id: String)
case class LiveScore(id: String, score: Double, gameSecondsRemaining: Int, playersYetToPlay: Int, playersCurrentlyPlaying: Int)
