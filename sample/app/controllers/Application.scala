package controllers

import play.api._
import play.api.Play.current
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Application extends Controller {

  def index = Action {
    Ok("Running :)")
  }

  def song(songId: String) = Action{
    Ok(s"Lalalala $songId")
  }
}