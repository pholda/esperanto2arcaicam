package pl.pholda.esperanto2arcaicam

import pl.pholda.esperanto2arcaicam.arcaicam._
import pl.pholda.esperanto2arcaicam.esperanto.{Subject, EoSentence, Vortspeco}
import pl.pholda.esperanto2arcaicam.esperanto.vortspeco._

import scala.annotation.tailrec

object Translator {
  import EoString.string2eoString

  def apply(sentence: EoSentence): String = {

    @tailrec
    def run(translated: List[String], words: List[Vortspeco], kazo: Kazo, difinaArtikolo: Boolean): List[String] = {
      words match {
        case DifinaArtikolo :: t =>
          run(translated, t, kazo, difinaArtikolo = true)
        case h :: t =>
          val k: Kazo = h match {
            //8a regulo
            case Prepozicio("de") => Genitivo
            case Prepozicio("da") => Genitivo
            case Prepozicio("dank'\u00A0al") => Dativo
            case Prepozicio("al") => Dativo
            case Prepozicio("malgraŭ") => Dativo
            case Prepozicio(_) => Nominativo
            case Substantivo(_, _, true) => Akuzativo
            case Adjektivo(_, _, true) => Akuzativo
            case _ => kazo
          }
          //1a regulo
          val artikolo = difinaArtikolo match {
            case true =>
              Some(s"ityu${
                if (h.isPlural == Some(true)) "y" else ""
              }${
                if (h.isAkuzativo == Some(true)) "n" else ""
              }")
            case _ =>
              None
          }
          val result = h match {
            //2a regulo
            case Substantivo(root, false, _) if k == Dativo =>
              root.a+"od"
            case Substantivo(root, true, _) if k == Dativo =>
              root.a+"oyd"
            case Substantivo(root, false, _) if k == Genitivo =>
              root.a+"es"
            case Substantivo(root, true, _) if k == Genitivo =>
              root.a+"eys"
            case Substantivo(root, false, false) =>
              root.a+"om"
            case Substantivo(root, true, false) =>
              root.a+"oy"
            case Substantivo(root, false, true) =>
              root.a+"on"
            case Substantivo(root, true, true) =>
              root.a+"oyn"
            //3a regulo
            case Adjektivo(root, false, _) if k == Dativo =>
              root.a+"ad"
            case Adjektivo(root, true, _) if k == Dativo =>
              root.a+"ayd"
            case Adjektivo(root, false, _) if k == Genitivo =>
              root.a+"es"
            case Adjektivo(root, true, _) if k == Genitivo =>
              root.a+"eys"
            case Adjektivo(root, false, false) =>
              root.a+"am"
            case Adjektivo(root, true, false) =>
              root.a+"ay"
            case Adjektivo(root, false, true) =>
              root.a+"an"
            case Adjektivo(root, true, true) =>
              root.a+"ayn"
            //4a regulo
            case Numeralo(n) =>
              def n2str(n: Int) = n match {
                case 0 => "nul"
                case 1 => "unn"
                case 2 => "dux"
                case 3 => "trid"
                case 4 => "cùar"
                case 5 => "cùin"
                case 6 => "sis"
                case 7 => "sep"
                case 8 => "oc"
                case 9 => "naù"
                case 10 => "dec"
                case 100 => "tzent"
                case 1000 => "mill"
                case _ => "???"
              }
              n match {
                case x if x >= 0 && x <= 10 => n2str(x)
                case x if x % 1000 == 0 => n2str(x/1000)+n2str(1000)
                case x if x % 100 == 0 => n2str(x/100)+n2str(100)
                case x if x % 10 == 0 => n2str(x/10)+n2str(10)
              }
            //6a regulo
            case Verbo(root, "i") if root.last == 'i' || root.last == 'e' =>
              root.a+"ar"
            case Verbo(root, "i") =>
              root.a+"ir"
            case Verbo(root, tense) if Seq("is", "as", "os", "us") contains tense =>
              val a = tense.head
              val b = sentence.subject match {
                case Subject(1, false) => "ms"
                case Subject(2, false) => "s"
                case Subject(3, false) => "t"
                case Subject(1, true) => "ims"
                case Subject(2, true) => "is"
                case Subject(3, true) => "it"
              }
              root.a+a+b
            case Verbo(root, "u") if !sentence.subject.plural =>
              root + "u"
            case Verbo(root, "u") if sentence.subject.plural =>
              root + "uy"

            case Verbo(root, tense) =>
              "["+root.a+tense+"]"
            //aliaj
            //korelativoj
            case Korelativo(a, b, j, n) =>
              val na = a match {
                case "ki" => "cuy"
                case "ti" => "ity"
                case "i" => "hey"
                case "neni" => "nemy"
                case "ĉi" => "chey"
              }
              val nb = b match {
                case "o" => "om"
                case "a" => "am"
                case "am" => "ahem"
                case "e" => "œ"
                case "om" => "ohem"
                case x => x
              }
              na+nb+{if (j) "y" else ""}+{if (n) "n" else ""}

            case Prepozicio(s) => s.a
            case Pronomo(_, _, _, false) => ""
            case Pronomo(pro, _, _, true) =>
              pro match {
                case "mi" => "mihin"
                case "ci" => "tuin"
                case "li" => "lùin"
                case "ŝi" => "eshin"
                case "ĝi" => "eghin"
                case "ni" => "nosin"
                case "vi" => "wosin"
                case "ili" => "ilùin"
                case "si" => "sihin"
                case s => s
              }
            case Unknown(v) => v
            case s => s.toString
          }

          val newTranslated = List(
            artikolo,
            result match {
              case "" => None
              case s => Some(s)
            }
          ).flatten

          run(translated ++ newTranslated, t, k, difinaArtikolo = false)
        case Nil => translated
      }
    }
    run(Nil, sentence.words.toList, Nominativo, difinaArtikolo = false).mkString(" ")
  }
}
