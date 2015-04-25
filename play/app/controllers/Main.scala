package controllers

import pl.pholda.esperanto2arcaicam.Translator
import pl.pholda.esperanto2arcaicam.esperanto.EoParser
import play.api.mvc._
import views.html

object Main extends Controller {

  def index = Action {
    Ok(html.index())
  }

  def translate = Action(parse.tolerantFormUrlEncoded) { implicit request =>
    val post = request.body
    Ok(Translator(EoParser(post("teksto").head).head))
  }
}
