package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Substantivo(root: String, plural: Boolean, akuzativo: Boolean) extends Vortspeco {
  override def isPlural: Option[Boolean] = Some(plural)

  override def isAkuzativo: Option[Boolean] = Some(akuzativo)
}

object Substantivo {
  private val r = "^(.+)o(j)?(n)?$".r

  val classify: PartialFunction[String, Vortspeco] = {
    case r(root, plural, akuzativo) =>
      Substantivo(root, plural == "j", akuzativo == "n")
  }
}
