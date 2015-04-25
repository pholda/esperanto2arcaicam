package pl.pholda.esperanto2arcaicam.esperanto.vortspeco

import pl.pholda.esperanto2arcaicam.esperanto.Vortspeco

/**
 *
 * @param root
 * @param tense (is|as|os|us|u|i)
 */
case class Verbo(root: String, tense: String) extends Vortspeco

object Verbo {
  private val r = "^(.+)(is|as|os|us|u|i)$".r

  val classify: PartialFunction[String, Vortspeco] = {
    case r(root, tense) =>
      Verbo(root, tense)
  }
}
