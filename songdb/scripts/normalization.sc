// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep com.ibm.icu:icu4j:78.1

import scala.collection.mutable.Buffer

import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern

import com.ibm.icu.text.Transliterator

import convert._

def generateNameVariants(name: String): Seq[String] = {
  var res = Seq[String]()
  val parts = name.split(" ").filter(_.nonEmpty)
  if (parts.length == 2) {
    val p0 = parts(0)
    val p1 = parts(1)
    if (p0.length > 0) res :+= s"${p0.substring(0, 1)}. $p1"
  } else if (parts.length >= 3) {
    val p0 = parts(0)
    val plast = parts.last
    if (p0.length > 0) {
      res :+= s"${p0.substring(0, 1)}. $plast"
      res :+= s"$p0 $plast"
    }
  }
  res
}

val transliteratorThreadLocal = new ThreadLocal[Transliterator] {
  override def initialValue(): Transliterator = 
    Transliterator.getInstance("NFD; [:Nonspacing Mark:] Remove; NFC; Any-Latin; Latin-ASCII")
}

val normalizeAuthorPatterns = Seq(
  " \\[2 musicians\\]$",
  "[^A-Za-z0-9]",
).map(Pattern.compile)
val normalizeAuthorCache = new ConcurrentHashMap[String, String]()
def normalizeAuthor(s: String): String = {
  if (s.isEmpty) s
  else {
    val cached = normalizeAuthorCache.get(s)
    if (cached != null) cached else {
      val lower = s.toLowerCase
      val transliterated = transliteratorThreadLocal.get().transliterate(lower)
      val res = normalizeAuthorPatterns.foldLeft(transliterated) { case (acc, pattern) =>
        pattern.matcher(acc).replaceAll("")
      }
      .replace('0','o')
      .replace('1','i')
      .replace('3','e')
      .replace('4','a')
      .replace('5','s')
      .replace('7','t')
      .trim
      normalizeAuthorCache.put(s, res)
      res
    }
  }
}

def normalizeName(name: String): String = transliteratorThreadLocal.get().transliterate(name)

val normalizePublisherPatterns = Seq(
  " company$",
  " consultants",
  " corp$",
  " corp\\.$",
  " corporation$",
  " creations$",
  " design$",
  " designs$",
  " development$",
  " developments$",
  " dezign$",
  " entertainment$",
  " games$",
  " gmbh$",
  " graphics$",
  " inc$",
  " inc\\.$",
  " interactive$",
  " international$",
  " limited$",
  " ltd$",
  " online$",
  " on-line$",
  " platinum$",
  " project$",
  " projects$",
  " productions$",
  " publishing$",
  " soft$",
  " software$",
  " studios$",
  " system$",
  " systems$",
  "[^A-Za-z0-9]",
).map(Pattern.compile)

val normalizePublisherCache = new ConcurrentHashMap[String, String]()
def normalizePublisher(s: String): String = {
  if (s.isEmpty) s
  else {
    val cached = normalizePublisherCache.get(s)
    if (cached != null) cached else {
      var lower = s.toLowerCase
      if (lower.startsWith("the "))
        lower = lower.substring(4)
      val transliterated = transliteratorThreadLocal.get().transliterate(lower)
      /*
      if (transliterated.replace(" ", "").trim.length >= 7) {
        val head = transliterated.trim.split(" ")(0)
        if (head.length >= 4) transliterated = head
      }
      */
      var res = normalizePublisherPatterns.foldLeft(transliterated) { case (acc, pattern) =>
        val res = pattern.matcher(acc).replaceAll("")
        if (res.isEmpty) acc else res
      }
      .replace('0','o')
      .replace('1','i')
      .replace('3','e')
      .replace('4','a')
      .replace('5','s')
      .replace('7','t')
      .trim
      // XXX
      if (s == "International Computer Entertainment") res = "ice"
      normalizePublisherCache.put(s, res)
      res
    }
  }
}

val normalizeAlbumPatterns = Seq(
  ("\\(.*\\)",""),
  (" PC$",""),
  (" ST$",""),
  (" - Falcon$",""),
  (" - Jaguar$",""),
  (" CD32$",""),
  (" AGA$",""),
  //(" GBC$",""),
  (" preview$", ""),
  (" [vV][0-9]+(\\.[0-9]+)*\\b",""), // TODO [vV] optional
  (" #(.*)$"," $1"),
  (" 0([1-9][0-9])$"," $1"),
  (" 00([0-9])$"," $1"),
  (" 0([0-9])$"," $1"),
  (" 0$",""),
  (" 1$",""),
  (" [Ii]$",""),
  (" [Ii][Ii]$"," 2"),
  (" [Ii][Ii][Ii]$"," 3"),
  (" [Ii][Vv]$"," 4"),
  (" [Vv]$"," 5"),
  (" [Vv][Ii]$"," 6"),
  (" [Vv][Ii][Ii]$"," 7"),
  (" [Vv][Ii][Ii][Ii]$"," 8"),
  (" [Ii][Xx]$"," 9"),
  ("^The ",""),
  ("^A ",""),
  ("^An ",""),
  ("^[Ff]irst ","1st "),
  ("^[Ss]econd ","2nd "),
  ("^[Tt]hird ","3rd "),
  ("^[Ff]ourth ","4th "),
  ("^[Ff]ifth ","5th "),
  ("^[Ss]ixth ","6th "),
  ("^[Ss]eventh ","7th "),
  ("^[Ee]ighth ","8th "),
  ("^[Nn]inth ","9th "),
  (" [Ff]irst "," 1st "),
  (" [Ss]econd "," 2nd "),
  (" [Tt]hird "," 3rd "),
  (" [Ff]ourth "," 4th "),
  (" [Ff]ifth "," 5th "),
  (" [Ss]ixth "," 6th "),
  (" [Ss]eventh "," 7th "),
  (" [Ee]ighth "," 8th "),
  (" [Nn]inth "," 9th "),
   // cracktro names
  (" PAL/NTSC Selector$",""),
  (" 100% \\(\\+.*\\)$",""),
  (" 100% \\+[0-9]+$",""),
  (" \\(\\+[0-9]+\\)$",""),
  (" \\+[0-9]+$",""),
  (" \\+\\+$",""),
).map { case (pattern, replacement) => (Pattern.compile(pattern), replacement) }
val normalizePattern2 = Pattern.compile("[^A-Za-z0-9\\.]")
val normalizeAlbumCache = new ConcurrentHashMap[(String, String, Buffer[String], Int), String]()
def normalizeAlbum(m: MetaData): String = normalizeAlbum(m._type, m.album, m.publishers, m.year)
def normalizeAlbum(_type: String, album: String, publishers: Buffer[String], year: Int): String = {
  if (album.isEmpty) return ""
  val key = (_type, album, publishers, year)
  val cached = normalizeAlbumCache.get(key)
  if (cached != null) cached else {
    var a = album
    val lca = album.toLowerCase
    val lctype = _type.toLowerCase
    if (lctype == "cracktro")
      a = a.trim + " [cracktro]"
    else if (lctype == "game" && !lca.startsWith("game ")) {
      if (lca.endsWith(" - demo"))
        a = a.substring(0, a.length - 7).trim
      else if (lca.endsWith(" - preview"))
        a = a.substring(0, a.length - 10).trim
      else if (lca.endsWith(" - final"))
        a = a.substring(0, a.length - 8).trim
      else if (lca.endsWith(" playable"))
        a = a.substring(0, a.length - 9).trim
      else if (lca.endsWith(" demo"))
        a = a.substring(0, a.length - 5).trim
      else if (lca.endsWith(" playable preview"))
        a = a.substring(0, a.length - 16).trim
      else if (lca.endsWith(" preview"))
        a = a.substring(0, a.length - 8).trim
      else if (lca.endsWith(" prev"))
        a = a.substring(0, a.length - 5).trim
      else if (lca.endsWith(" beta"))
        a = a.substring(0, a.length - 5).trim
      else if (lca.matches(" \\(.*playable.*\\)$"))
        a = a.replaceAll(" \\(.*playable.*\\)$", "").trim
      else if (lca.matches(" \\(.*demo.*\\)$"))
        a = a.replaceAll(" \\(.*demo.*\\)$", "").trim
      else if (lca.matches(" \\(.*preview.*\\)$"))
        a = a.replaceAll(" \\(.*preview.*\\)$", "").trim
      else if (lca.matches(" \\(.*beta.*\\)$"))
        a = a.replaceAll(" \\(.*beta.*\\)$", "").trim
      else if (lca.matches(" \\(.*version.*\\)$"))
        a = a.replaceAll(" \\(.*version.*\\)$", "").trim
    }
    publishers.foreach(p =>
      a = a.replaceAll(s"^(?i)${Pattern.quote(p.toLowerCase)} ", "")
    )
    a = a.replaceAll(" - .*", "")
         .replaceAll(": .*", "")
    //.replaceAll("- .*","")
    //.replaceAll("/ .*","")
    //.replaceAll(": .*","")
    //.trim
    var normalized = normalizeAlbumPatterns.foldLeft(a) { case (acc, (pattern, replacement)) =>
      pattern.matcher(acc).replaceAll(replacement)
    }.toLowerCase

    if (year > 0 && normalized.endsWith(s" $year"))
      normalized = normalized.substring(0, normalized.length - 5).trim

    val transliterated = transliteratorThreadLocal.get().transliterate(normalized)
    val res = normalizePattern2.matcher(transliterated).replaceAll("").trim
    normalizeAlbumCache.put(key, res)
    res
  }
}
def _normalizeAlbum(s: String) =
  s.toLowerCase
    .replaceAll("\\(.*\\)", "")
    .replaceAll("[^a-z0-9]", "")

def normalizeRealName(realName: String, handle: String): Option[String] = {
  val handleparts = handle.split(" ").map(_.toLowerCase)
  var realnameparts = realName.split(" ")
  if (realnameparts.last == "Jr.")
    realnameparts = realnameparts.dropRight(1)
  val lcrealnameparts = realnameparts.map(_.toLowerCase)
  var realname =
    if (handleparts.last == lcrealnameparts.last && (handle.length != (realnameparts.head + " " + realnameparts.last).length || realnameparts.exists(_.contains("."))))
      None
    else if (lcrealnameparts.length >= 3 && !lcrealnameparts.contains("van") && !lcrealnameparts.contains("del") && !lcrealnameparts.exists(_.contains(".")))
      Some(realnameparts.head + " " + realnameparts.last)
    else Some(realName)
  // XXX
  if (realname == Some("Haikko Ruttmann")) realname = Some("Haiko Ruttmann")
  realname
}

def isPreview(lcalbum: String): Boolean =
  !lcalbum.startsWith("game ") && (lcalbum.endsWith(" preview") || lcalbum.endsWith(" prev") || lcalbum.endsWith(" demo") || lcalbum.endsWith(" beta") || lcalbum.endsWith(" (preview)") || lcalbum.endsWith(" (demo)") || lcalbum.endsWith(" (beta)") || lcalbum.endsWith(" version)"))

def isCracktro(_type: String): Boolean = _type.toLowerCase == "cracktro" || _type.toLowerCase == "crack intro" || _type.toLowerCase == "import intro" || _type.toLowerCase == "fix/patch Intro" || _type.toLowerCase == "trainer"

def normalizeType(s: String): String = s.toLowerCase match {
  case "" => ""
  case "compo" => "Compo"
  case "game" => "Game"
  case _ if isCracktro(s) => "Cracktro"
  case _ => "Other"
}

def normalizeFilename(filename: String): String = filename.toLowerCase.replaceAll("[^a-z0-9/]", "")
