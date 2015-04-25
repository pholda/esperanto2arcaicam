package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Korelativo(a: String, b: String, plural: Boolean, akuzativo: Boolean) extends Vortspeco

object Korelativo {
  private val r = "^(ĉi|i|ki|neni|ti)(a|al|am|e|el|es|o|om|u)(j)?(n)?$".r

  val classify: PartialFunction[String, Vortspeco] = {
    case r(a, b, j, n) =>
      Korelativo(a, b, j == "j", n == "n")
  }
}
