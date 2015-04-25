package pl.pholda.esperanto2arcaicam.esperanto

import org.scalatest.FunSuite
import pl.pholda.esperanto2arcaicam.Translator

class TranslationTest extends FunSuite {
  test("blabla") {
    println(Translator(EoParser("la biero estas bongusta").head))
    println(Translator(EoParser("mi ŝatas la bierojn").head))
    println(Translator(EoParser("ĉevalo ŝatas havi ĉevalojn").head))
    println(Translator(EoParser("ĉevaloj ŝatas havi ĉevalojn").head))
    println(Translator(EoParser("biero de homo estas forta").head))
    println(Translator(EoParser("mi drinkas bieron kiun mi ŝatas").head))
    println(Translator(EoParser("mi drinkas bierojn kiujn mi ŝatas").head))
    println(Translator(EoParser("pompa esperanto junularo").head))
  }

}
