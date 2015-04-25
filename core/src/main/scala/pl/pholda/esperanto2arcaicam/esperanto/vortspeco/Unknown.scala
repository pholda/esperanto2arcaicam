package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Unknown(value: String) extends Vortspeco

object Unknown {
  val classify: PartialFunction[String, Vortspeco] = {
    case s => Unknown(s)
  }
}
