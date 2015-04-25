package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

case class Adjektivo(root: String, plural: Boolean, akuzativo: Boolean) extends Vortspeco {
  override def isPlural: Option[Boolean] = Some(plural)

  override def isAkuzativo: Option[Boolean] = Some(akuzativo)
}

object Adjektivo {
  private val r = "^(.+)a(j)?(n)?$".r

  val classify: PartialFunction[String, Vortspeco] = {
    case r(root, plural, akuzativo) =>
      Adjektivo(root, plural == "j", akuzativo == "n")
  }
}
