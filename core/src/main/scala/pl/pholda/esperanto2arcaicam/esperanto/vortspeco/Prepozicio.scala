package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Prepozicio(a: String) extends Vortspeco

object Prepozicio {
  private val prepozicioj =
    Set("al", "anstataŭ", "apud", "ĉe", "ĉirkaŭ", "de", "da", "dum", "ekster", "el",
    "en", "ĝis", "inter", "je", "kontraŭ", "krom", "kun", "laŭ", "malgraŭ", "per", "por", "po", "post",
    "preter", "pri", "pro", "sen", "sub", "super", "sur", "tra", "trans")

  val classify: PartialFunction[String, Vortspeco] = {
    case s if prepozicioj contains s => Prepozicio(s)
  }
}
