package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case object DifinaArtikolo extends Vortspeco {
  val classify: PartialFunction[String, Vortspeco] = {
    case "la" =>
      DifinaArtikolo
  }
}
