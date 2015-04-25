package pl.pholda.esperanto2arcaicam.esperanto

import pl.pholda.esperanto2arcaicam.esperanto.vortspeco._

object EoParser {

  lazy val classifier: PartialFunction[Word, Vortspeco] =
    Pronomo.classify orElse
    Numeralo.classify orElse
    DifinaArtikolo.classify orElse
    Prepozicio.classify orElse
    Korelativo.classify orElse
    Substantivo.classify orElse
    Adjektivo.classify orElse
    Verbo.classify orElse
    Unknown.classify


  def apply(str: String): Seq[EoSentence] = {
    str.split('.').map{s =>
      EoSentence(
        s.split(' ').map{w =>
        classifier(w)
      }.toSeq)
    }.toSeq
  }
}
