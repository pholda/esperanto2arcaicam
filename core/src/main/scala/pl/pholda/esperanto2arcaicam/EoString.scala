package pl.pholda.esperanto2arcaicam

case class EoString(s: String) extends AnyVal {
  def arcaicamLetters: String = {
    s.replace("c", "cz")
      .replace("ĉ", "ch")
      .replace("f", "ph")
      .replace("ge", "gue")
      .replace("gi", "gui")
      .replace("ĝ", "gh")
      .replace("ĥ", "qh")
      .replace("j", "y")
      .replace("ĵ", "zh")
      .replace("ke", "que")
      .replace("ki", "qui")
      .replace("k", "c")
      .replace("ŝ", "sh")
      .replace("ŭ", "ù")
      .replace("v", "w")
      .replace("dz", "zz")
      .replace("ks", "x")
      .replace("kv", "cù")
  }

  def arcaicamVorteto: PartialFunction[String, String] = {
    case "adiaŭ" => "adiez"
    case "al" => "ad"//"ad(i)"
    case "almenaŭ" => "almenez"
    case "ambaŭ" => "ambez"
    case "ankaŭ" => "anquez"
    case "ankoraŭ" => "ancorez"
    case "anstataŭ" => "anstatez"
    case "antaŭ" => "antez"
//      case "antaŭ (loko)" => "antez"
//      case "antaŭ (tempo)" => "prezz"
    case "apenaŭ" => "apenez"
//      case "aŭ" => "aù(die)"
    case "aŭ" => "aù"
    case "baldaŭ" => "baldez"
    case "ĉe" => "chez"
    case "ĉirkaŭ" => "chirquez"
//        ĉu: chu(des) - ĉu vere?: werœ? - ĉu ne?: phalsœ?
    case "ĉu" => "chu"
    case "ĉu\u00A0vere" => "werœ"
    case "ĉu\u00A0ne" => "phalsœ"
    case "da" => "del"
    case "dank'\u00A0al" => "grez"// (aŭ dancu)"
    case "de" => "del"// (2)"
    case "des" => "deste"
    case "do" => "des"// (aŭ -die)"
    case "dum" => "dum"//(quez)"
    case "eĉ" => "eche"
    case "ekde" => "ab"//(u)"
    case "ekster" => "extrum"
    case "el" => "ex"// (kiel la prefikso "eks")"
    case "en" => "in"
    case "ĝis" => "ghisquez"
    case "hieraŭ" => "hierez"
    case "hodiaŭ" => "hodiez"
    case "ja" => "yad"
    case "jam" => "yamen"
    case "je" => "iyed"
    case "jen" => "yemen"
    case "jes" => "ayest"
    case "ju" => "yud"
    case "ĵus" => "zhused"
    case "kaj" => "ed"
    case "kontraŭ" => "contrez"
    case "kun" => "cum"
    case "kvankam" => "cùanquez"
    case "kvazaŭ" => "cùazes"
    case "laŭ" => "selez"
    case "malantaŭ" => "postez"
//      case "malantaŭ (loko)" => "postez"
//      case "malantaŭ (tempo)" => "post"
    case "malgraŭ" => "malgrez"
    case "mem" => "memes"
    case "morgaŭ" => "morguez"
    case "ne" => "ned"//(œ)"
    case "nepre" => "nepred"
    case "plej" => "pluy"
    case "pli" => "plid"
    case "plu" => "plud"
    case "po" => "pod"
    case "post" => "postez"
//      case "post (loko)" => "postez"
//      case "post (tempo)" => "post"
    case "preskaŭ" => "presquez"
    case "preter" => "predor"
    case "pri" => "prid"
    case "pro" => "pru"
    case "sen" => "sonz"
    case "sub" => "sobez"
    case "supre" => "suprez"
    case "sur" => "sobrez"
    case "tre" => "trez"
  }

  def a: String = {
    arcaicamVorteto.lift.apply(s).getOrElse(arcaicamLetters)
  }
}

object EoString {
  implicit def string2eoString(s: String): EoString = EoString(s)
}