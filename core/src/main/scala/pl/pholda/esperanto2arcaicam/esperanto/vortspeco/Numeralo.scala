package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Numeralo(n: Int) extends Vortspeco {

}

object Numeralo {
  private val r = "^(nul|unu|du|tri|kvar|kvin|ses|sep|ok|naŭ)?(dek|cent|mil)?$".r

  val classify: PartialFunction[String, Vortspeco] = {
    case r(a, b) =>
      val na = a match {
        case "unu" => 1
        case "du" => 2
        case "tri" => 3
        case "kvar" => 4
        case "kvin" => 5
        case "ses" => 6
        case "sep" => 7
        case "ok" => 8
        case "naŭ" => 9
        case "nul" => 0
        case _ => 1
      }
      val nb = b match {
        case "dek" => 10
        case "cent" => 100
        case "mil" => 1000
        case _ => 1
      }
      Numeralo(na*nb)
  }
}
