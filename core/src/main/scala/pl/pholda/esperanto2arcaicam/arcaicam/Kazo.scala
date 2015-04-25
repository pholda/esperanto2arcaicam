package pl.pholda.esperanto2arcaicam.arcaicam

sealed abstract class Kazo

case object Nominativo extends Kazo

case object Akuzativo extends Kazo

case object Dativo extends Kazo

case object Genitivo extends Kazo
