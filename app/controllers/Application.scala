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

class Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def admin = Action {
    getFranchiseList.fold(InternalServerError("error getting list of franchises")) { franchises =>
      Ok(views.html.admin(adminForm, franchises))
    }
  }

  def adminPost = Action { implicit request =>
    adminForm.bindFromRequest.fold(
      formWithErrors => {
        val franchiseList = getFranchiseList.getOrElse(List())
        BadRequest(views.html.admin(formWithErrors, franchiseList))
      },
      adminData => {
        Redirect(routes.Application.index())
      }
    )
  }

  def getFranchiseList: Option[List[Franchise]] = {
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

  def test = Action {
    Ok(views.html.index())
  }

  val adminForm = Form(
    mapping(
      "advancers" -> number(min = 0, max = 100),
      "teamId" -> list(nonEmptyText)
    )(AdminData.apply)(AdminData.unapply)
  )
}

case class AdminData(advancerCount: Int, teamIds: List[String])
case class Franchise(name: String, id: String)