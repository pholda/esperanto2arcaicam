package pl.pholda.esperanto2arcaicam.esperanto

import pl.pholda.esperanto2arcaicam.esperanto.vortspeco.{Substantivo, Pronomo}

case class EoSentence(words: Seq[Vortspeco]) {
  lazy val subject: Subject = {
    words.find{
      _.isInstanceOf[Pronomo]
    }.map{
      case Pronomo(_, person, plural, _) => Subject(person, plural)
    }.getOrElse{
      words.find{
        case Substantivo(_, _, false) => true
        case _ => false
      }.map{
        case Substantivo(_, plural, _) => Subject(3, plural)
      }.getOrElse(Subject(3, false))
    }
  }
}
