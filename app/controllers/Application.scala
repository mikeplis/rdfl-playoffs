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

class Application extends Controller {

  lazy val redis = new RedisClient("localhost", 6379)

  lazy val adminForm = Form(
    mapping(
      "advancers" -> number(min = 0, max = 100),
      "teamId" -> list(nonEmptyText)
    )(AdminData.apply)(AdminData.unapply)
  )

  val TeamIds = "teamIds"

  def index = Action {
    //redis.lrange(TeamIds, 0, -1): Option[List[Option[String]]]
    Ok(views.html.index())
  }

  def test = Action {
    // main page using redis data, but fake scores
    Ok(views.html.index())
  }

  def admin = Action {
    getFranchiseListFromMFL.fold(InternalServerError("error getting list of franchises")) { franchises =>
      Ok(views.html.admin(adminForm, franchises))
    }
  }

  def adminPost = Action { implicit request =>
    adminForm.bindFromRequest.fold(
      formWithErrors => {
        val franchiseList = getFranchiseListFromMFL.getOrElse(List())
        BadRequest(views.html.admin(formWithErrors, franchiseList))
      },
      adminData => {
        redis.set("advancerCount", adminData.advancerCount)
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