package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

class Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def admin = Action {
    Ok(views.html.admin(adminForm))
  }

  def adminPost = Action { implicit request =>
    adminForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.admin(formWithErrors))
      },
      adminData => {
        Redirect(routes.Application.index())
      }
    )
  }

  val adminForm = Form(
    mapping(
      "teams" -> nonEmptyText,
      "advancers" -> number(min = 0, max = 100)
    )(AdminData.apply)(AdminData.unapply)
  )
}

case class AdminData(teamIds: String, advancerCount: Int)
