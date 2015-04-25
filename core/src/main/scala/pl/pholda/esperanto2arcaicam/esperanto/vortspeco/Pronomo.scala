package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Pronomo(pronoun: String, person: Int, plural: Boolean, akuzativo: Boolean) extends Vortspeco {
  override def isPlural: Option[Boolean] = Some(plural)

  override def isAkuzativo: Option[Boolean] = Some(akuzativo)
}

object Pronomo {
  private val r = "^(mi|vi|li|ŝi|ĝi|ni|ili|oni|si|ci)(n)?$".r

  val classify: PartialFunction[String, Pronomo] = {
    case r(pronoun, n) =>
      Pronomo(
        pronoun,
        pronoun match {
          case "mi" => 1
          case "ni" => 1
          case "vi" => 2
          case "ci" => 2
          case "ili" => 2
          case _ => 3
        },
        pronoun match {
          case "ni" => true
          case "vi" => true
          case "ili" => true
          case "oni" => true
          case _ => false
        },
        n == "n"
      )
  }
}
