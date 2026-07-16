// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Matti Tiainen <mvtiaine@cc.hut.fi>
// parsing code generated with help from various LLMs

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep net.ruippeixotog::scala-scraper::3.1.0

import java.net.URLDecoder
import java.nio.file.Files
import java.nio.file.Paths
import java.util.regex.Pattern
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Try
import scala.util.Using

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

import convert._
import normalization._

final case class KestraReleaseRef(
  id: Option[Int],
  title: String
)

final case class KestraDownload (
  url: String,
  filename: String,
  fileType: Option[String],
  filesize: Option[Int],
  compressor: Option[KestraReleaseRef],
  crc: Option[String],
  md5: Option[String] = None,
  group: String = ""
)

final case class KestraAuthor (
  id: Option[Int],
  name: String,
  country: Option[String],
  role: String,
  group: String = ""
)

final case class KestraParty (
  id: Int,
  name: String,
)

final case class KestraRelease (
  id: Option[Int],
  title: String,
  _type: String,
  tags: Seq[String],
  group: String,
  containedAs: Option[KestraReleaseRef] = None,
  from: Option[KestraReleaseRef] = None
)

final case class KestraSeries (
  id: Int,
  name: String,
  issue: Option[String],
)

final case class KestraAuthorRef (
  id: Int,
  name: String,
  activeStart: Option[Int] = None,
  activeEnd: Option[Int] = None
)

final case class KestraAlias (
  name: String,
  activeStart: Option[Int],
  activeEnd: Option[Int]
)

final case class KestraAuthorMeta (
  id: Int,
  name: String,
  altName: Option[String],
  entityType: String,
  realName: Option[String],
  roles: Seq[String],
  activeStart: Option[Int],
  activeEnd: Option[Int],
  country: Option[String],
  aliases: Seq[KestraAlias],
  groups: Seq[KestraAuthorRef],
  exSysop: Seq[KestraAuthorRef],
  alsoListed: Seq[KestraAuthorRef],
  phone: Option[String],
  memberCountries: Seq[String],
  organizedParties: Seq[KestraAuthorRef],
  publishedSeries: Seq[KestraAuthorRef]
)

final case class KestraMeta (
  id: Int,
  title: String,
  types: Seq[String],
  categories: Seq[String],
  party: Option[KestraParty],
  series: Option[KestraSeries],
  authors: Seq[KestraAuthor],
  credits: Seq[KestraAuthor],
  released: Option[String],
  madeOrFinished: Option[String],
  competition: Option[String],
  rank: Option[Int],
  playingTime: Option[String],
  soundStyle: Seq[String],
  soundFormat: Option[String],
  tags: Seq[String],
  origin: Option[KestraReleaseRef],
  downloads: Seq[KestraDownload],
  features: Seq[KestraRelease],
  connections: Seq[KestraRelease],
)

val kestra_path = System.getProperty("user.home") + "/sources/metadata/kestra/"

def parsePlayingTime(t: String): Int = {
  val timeRegex = """(?:(\d+):)?(\d+):(\d+)""".r
  timeRegex.findFirstMatchIn(t) match {
    case Some(m) =>
      val hours = Option(m.group(1)).map(_.toInt).getOrElse(0)
      val minutes = m.group(2).toInt
      val seconds = m.group(3).toInt
      hours * 3600 + minutes * 60 + seconds
    case None =>
      0
  }
}

def parseKestraMeta(id: Int, elem: Element): Option[KestraMeta] = {
  val h1Links = elem.select("h1 a")
  if (h1Links.isEmpty) return None

    // First link is title, rest are authors
    val title = h1Links.head.text
    
    val h1Text = elem.select("h1").head.text
    val titleEndIdx = h1Text.indexOf(title) + title.length
    val afterTitle = h1Text.substring(titleEndIdx).trim
    val _typeStr = if (afterTitle.startsWith("(")) {
      afterTitle.substring(1, afterTitle.indexOf(")"))
    } else {
      "unknown"
    }
    val types = _typeStr.split("/").map(_.trim).toSeq

    // Parse authors with country information
    val authorsMap = scala.collection.mutable.Map[String, KestraAuthor]()
    h1Links.foreach { link =>
      val href = link.attr("href")
      if (href.contains("author.php")) {
        val authorName = link.text.replaceAll("\\s*\\([^)]+\\)", "").trim
        val authorId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt)
        var country: Option[String] = None
        var role = "unknown"
        
        authorsMap(authorName) = KestraAuthor(authorId, authorName, country, role)
      }
    }

    var released: Option[String] = None
    var madeOrFinished: Option[String] = None
    var competition: Option[String] = None
    var rank: Option[Int] = None
    var playingTime: Option[String] = None
    var soundStyle: Seq[String] = Seq.empty
    var soundFormat: Option[String] = None
    var origin: Option[KestraReleaseRef] = None
    var categories: Seq[String] = Seq.empty
    var party: Option[KestraParty] = None
    var series: Option[KestraSeries] = None
    val tags = Buffer[String]()

    val rankRegex = """, ranked (\d+) in the (.+)$""".r
    val unselectedRegex = """, unselected in the (.+)$""".r

    elem.select("ul.nodots li").foreach { li =>
      val text = li.text
      if (text.startsWith("Released:")) {
        val releasedStr = li.select("em a").headOption.map(_.text).orElse(li.select("em.blacky").headOption.map(_.text)).getOrElse(text.replace("Released: ", "").trim.split("\\s+")(0)).trim
        released = if (releasedStr.nonEmpty) Some(releasedStr) else None
        val partyLink = li.select("a[href*='party.php']").headOption
        if (partyLink.isDefined && !text.contains(" after " + partyLink.get.text)) {
          val partyId = """id=(\d+)""".r.findFirstMatchIn(partyLink.get.attr("href")).map(_.group(1).toInt)
          val partyName = Some(partyLink.get.text)
          party = Some(KestraParty(partyId.getOrElse(0), partyName.getOrElse("Unknown")))
        }

        rankRegex.findFirstMatchIn(text) match {
          case Some(m) =>
            rank = Some(m.group(1).toInt)
            competition = Some(m.group(2))
          case None =>
            unselectedRegex.findFirstMatchIn(text) match {
              case Some(m) => competition = Some(m.group(1))
              case None =>
            }
        }
      } else if (text.startsWith("Made or finished:")) {
        madeOrFinished = Some(text.replace("Made or finished:", "").trim)
      } else if (text.startsWith("In Series:")) {
        val seriesLink = li.select("a[href*='series.php']").headOption
        if (seriesLink.isDefined) {
          val seriesId = """id=(\d+)""".r.findFirstMatchIn(seriesLink.get.attr("href")).map(_.group(1).toInt)
          val seriesName = Some(seriesLink.get.text)
          var seriesIssue: Option[String] = None
          val issueMatch = """issue:\s*([^.]+)""".r.findFirstMatchIn(text)
          if (issueMatch.isDefined) {
            seriesIssue = Some(issueMatch.get.group(1).trim)
          } else {
            val bElems = li.select("b").toSeq
            if (bElems.size >= 2) seriesIssue = Some(bElems(1).text)
          }
          series = Some(KestraSeries(seriesId.getOrElse(0), seriesName.getOrElse("Unknown"), seriesIssue))
        }
      } else if (text.startsWith("Playing Time:")) {
        playingTime = Some(text.replace("Playing Time: ", "").trim)
      } else if (text.startsWith("Sound Style:")) {
        val styleLinks = li.select("a").toSeq
        if (styleLinks.nonEmpty) {
          soundStyle = styleLinks.map(_.text)
        } else {
          val splits = text.replace("Sound Style:", "").trim.split("/").map(_.trim).filter(_.nonEmpty)
          soundStyle = if (splits.nonEmpty) splits.toSeq else Seq(text.replace("Sound Style:", "").trim)
        }
      } else if (text.startsWith("Sound Format:")) {
        soundFormat = Some(text.replace("Sound Format: ", "").trim)
      } else if (text.startsWith("Origin:")) {
        val originLink = li.select("a").headOption
        if (originLink.isDefined) {
          val href = originLink.get.attr("href")
          val originId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt)
          val originText = originLink.get.text
          origin = Some(KestraReleaseRef(originId, originText))
        } else {
          origin = Some(KestraReleaseRef(None, text.replace("Origin: ", "").trim))
        }
      } else if (text.startsWith("Categorized as:")) {
        val catLinks = li.select("a")
        if (catLinks.nonEmpty) {
          categories = catLinks.map(_.text).toSeq
        } else {
          categories = Seq(text.replace("Categorized as: ", "").trim.split("\\s+")(0))
        }
      }
    }

    elem.select("small.tags a").foreach { tag =>
      tags += tag.text
    }

    // Parse downloads
    val downloads = Buffer[KestraDownload]()
    elem.select("div#downloads div.group").foreach { group =>
      val h3Text = group.select("h3").headOption.map(_.text).getOrElse("")
      group.select("li").foreach { li =>
        val links = li.select("a").toSeq
        val fileLink = links.find(l => !l.attr("href").contains("file.php") && !l.attr("href").contains("release.php")).getOrElse(links.headOption.orNull)
        if (fileLink != null) {
          val href = fileLink.attr("href")
          val filename = fileLink.text
          var fileType: Option[String] = None
          var filesize: Option[Int] = None
          var compressor: Option[KestraReleaseRef] = None
          var crc: Option[String] = None
          var md5: Option[String] = None
          
          val liText = li.text
          val fileTypeMatch = "\\(([^)]+)\\),\\s*[0-9]+\\s+bytes".r.findFirstMatchIn(liText)
          if (fileTypeMatch.isDefined) {
            fileType = Some(fileTypeMatch.get.group(1).trim)
          }

          val sizeMatch = "([0-9]+)\\s+bytes".r.findFirstMatchIn(liText)
          if (sizeMatch.isDefined) {
            filesize = Some(sizeMatch.get.group(1).toInt)
          }
          val crcMatch = "CRC:\\s+([A-F0-9]+)".r.findFirstMatchIn(liText)
          if (crcMatch.isDefined) {
            crc = Some(crcMatch.get.group(1))
          }

          val md5Match = "(?:[?&])md5=([A-Fa-f0-9]{32})".r.findFirstMatchIn(links.map(_.attr("href")).mkString(" "))
          if (md5Match.isDefined) {
            md5 = Some(md5Match.get.group(1).toLowerCase)
          }

          val compLink = links.find(l => l.attr("href").contains("release.php") && liText.contains(s"(${l.text})"))
          if (compLink.isDefined) {
            val l = compLink.get
            val cId = l.attr("href").replaceAll("[^0-9]", "").toIntOption
            compressor = Some(KestraReleaseRef(cId, l.text))
          } else {
            val matchNotCrunched = """\((not crunched.*?)\)""".r.findFirstMatchIn(liText)
            if (matchNotCrunched.isDefined) {
              compressor = Some(KestraReleaseRef(None, matchNotCrunched.get.group(1).trim))
            }
          }
          
          downloads += KestraDownload(href, filename, fileType, filesize, compressor, crc, md5, h3Text)
        }
      }
    }

    // Parse credits
    val credits = Buffer[KestraAuthor]()
    elem.select("div#credits div.group").foreach { groupDiv =>
      var currentGroup = "unknown"
      var currentRole = "unknown"
      groupDiv.children.foreach { child =>
        if (child.tagName == "h3") {
           currentGroup = child.text
        } else if (child.tagName == "h4") {
          currentRole = child.text
        } else if (child.tagName == "ul") {
          child.select("li a[href*='author.php']").foreach { a =>
            val href = a.attr("href")
            val authorId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt)
            val authorName = a.text.replaceAll("\\s*\\([^)]+\\)", "").trim
            credits += KestraAuthor(authorId, authorName, None, currentRole, currentGroup)
          }
        }
      }
    }

    // Parse featured in / release features
    val features = Buffer[KestraRelease]()
    val featuredIn = Buffer[KestraRelease]()
    elem.select("div.group:has(table.small_list)").foreach { groupDiv =>
      val h3Text = groupDiv.select("h3").headOption.map(_.text).getOrElse("")
      val table = groupDiv.select("table.small_list tr")
      table.foreach { tr =>
        val aLink = tr.select("span.titletags a").headOption
        val strongTag = tr.select("span.titletags strong").headOption
        val safeTitleLink = aLink.orElse(strongTag)
        if (safeTitleLink.isDefined) {
          val link = safeTitleLink.get
          val featureTitle = link.text
          val featureHref = if (link.tagName == "a") link.attr("href") else ""
          val releaseId = if (featureHref.nonEmpty) featureHref.replaceAll("[^0-9]", "").toIntOption else None
          val typeElem = tr.select("td.cat_td").headOption
          
          var featureType = "unknown"
          var tags = Seq.empty[String]
          
          if (typeElem.isDefined) {
            val te = typeElem.get
            val span = te.select("> span.nbsp").headOption
            if (span.isDefined) {
              featureType = span.get.ownText.trim
            } else {
              featureType = te.ownText.trim
            }
            val small = te.select("> small").headOption
            if (small.isDefined) {
              tags = small.get.text.split(",").flatMap(_.split(" - ")).map(t => t.stripPrefix("-").trim).filter(_.nonEmpty).toSeq
            }
          }
          if (featureType.isEmpty) featureType = "unknown"
          
          var containedAs: Option[KestraReleaseRef] = None
          val tds = tr.select("td").toSeq
          if (tds.length >= 4) {
            val containedTd = tds(3)
            val cLink = containedTd.select("a").headOption
            if (cLink.isDefined) {
              val cId = cLink.get.attr("href").replaceAll("[^0-9]", "").toIntOption
              val cTitle = cLink.get.text
              if (cTitle.nonEmpty) containedAs = Some(KestraReleaseRef(cId, cTitle))
            }
          }

          var fromRef: Option[KestraReleaseRef] = None
          tr.select("span.titletags small").foreach { smallElem =>
            if (smallElem.text.contains("from ")) {
              val fLink = smallElem.select("a").headOption
              if (fLink.isDefined) {
                val fId = fLink.get.attr("href").replaceAll("[^0-9]", "").toIntOption
                val fTitle = fLink.get.text
                if (fTitle.nonEmpty) fromRef = Some(KestraReleaseRef(fId, fTitle))
              }
            }
          }

          val f = KestraRelease(releaseId, featureTitle, featureType, tags, h3Text, containedAs, fromRef)
          if (h3Text == "Release Features") features += f else featuredIn += f
        }
      }
    }

    /// XXX quirks
    if (title == "Bazza 'n' Runt" && types.contains("Game")) released = Some("1994")
    else if (title == "Drivin' Force" && types.contains("Game")) released = Some("1990")
    else if (title == "Trax" && types.contains("Game")) released = Some("1993")
    else if (title == "PTC" && types.contains("Game")) released = Some("1987")
    else if (title == "Fate - Gates Of Dawn" && types.contains("Game")) released = Some("1991")
    else if (title == "Flyin' High" && types.contains("Game")) released = Some("1997")
    else if (title == "Master Ninja 2" && types.contains("Game")) released = Some("1988")
    else if (title == "Delta Run" && types.contains("Game")) released = Some("1989")

    Some(KestraMeta(
      id = id,
      title = title,
      types = types,
      categories = categories,
      party = party,
      series = series,
      authors = authorsMap.values.toSeq,
      credits = credits.toSeq,
      released = released,
      madeOrFinished = madeOrFinished,
      competition = competition,
      rank = rank,
      playingTime = playingTime,
      soundStyle = soundStyle,
      soundFormat = soundFormat,
      tags = tags.toSeq,
      origin = origin,
      downloads = downloads.toSeq,
      features = features.toSeq,
      connections = featuredIn.toSeq
    ))
}

val releases = Files.list(Paths.get(kestra_path + "release/")).toScala(Buffer).par.flatMap(f =>
  val doc = JsoupBrowser().parseFile(f.toFile)
  val data = doc >> elementList("#content")
  if (data.length > 0) {
    val id = f.toString().split("=").last.toInt
    val meta = parseKestraMeta(id, data(0))
    meta.foreach { meta =>
      println(s"""
=== Kestra Release ID: ${meta.id} ===
Title: ${meta.title}
Types: ${meta.types.mkString(", ")}
Categories: ${if (meta.categories.nonEmpty) meta.categories.mkString(", ") else "N/A"}
Party: ${meta.party.map(p => s"${p.name} [ID: ${p.id}]").getOrElse("N/A")}
Series: ${meta.series.map(s => s"${s.name} [ID: ${s.id}], issue: ${s.issue}").getOrElse("N/A")}
Authors: ${meta.authors.map(a => s"${a.name} [ID: ${a.id.getOrElse("?")}]").mkString(", ")}
Credits:
  ${meta.credits.groupBy(_.group).map { case (g, lst) =>
    s"[$g]\n  " + lst.map(c => s"${c.name} [ID: ${c.id.getOrElse("?")}] (${c.role})").mkString("\n  ")
  }.mkString("\n  ")}
Released: ${meta.released.getOrElse("N/A")}
Made or Finished: ${meta.madeOrFinished.getOrElse("N/A")}
Competition: ${meta.competition.getOrElse("N/A")}
Rank: ${meta.rank.map(_.toString).getOrElse("N/A")}
Playing Time: ${meta.playingTime.getOrElse("N/A")}
Sound Style: ${if (meta.soundStyle.nonEmpty) meta.soundStyle.mkString(", ") else "N/A"}
Sound Format: ${meta.soundFormat.getOrElse("N/A")}
Tags: ${if (meta.tags.nonEmpty) meta.tags.mkString(", ") else "N/A"}
Origin: ${meta.origin.map(o => s"${o.title} [ID: ${o.id.getOrElse("?")}]").getOrElse("N/A")}
Downloads: ${meta.downloads.length}
  ${meta.downloads.groupBy(_.group).map { case (g, lst) =>
    s"[$g]\n  " + lst.map(d => s"${d.filename} [type: ${d.fileType.getOrElse("?")}, comp: ${d.compressor.map(c => s"${c.title} [ID: ${c.id.getOrElse("?")}]").getOrElse("?")}] (${d.filesize.map(f => s"${f} bytes").getOrElse("unknown")}, CRC: ${d.crc.getOrElse("N/A")}) - URL: ${d.url}").mkString("\n  ")
  }.mkString("\n  ")}
Features: ${meta.features.length}
  ${meta.features.groupBy(_.group).map { case (g, lst) =>
    s"[$g]\n  " + lst.map(f => s"${f.title} [ID: ${f.id.getOrElse("?")}] (${f._type}${if(f.tags.nonEmpty) " - Tags: " + f.tags.mkString(", ") else ""})${f.containedAs.map(c => s" - contained as: ${c.title} [ID: ${c.id.getOrElse("?")}]").getOrElse("")}${f.from.map(fr => s" - from: ${fr.title} [ID: ${fr.id.getOrElse("?")}]").getOrElse("")}").mkString("\n  ")
  }.mkString("\n  ")}
Connections: ${meta.connections.length}
  ${meta.connections.groupBy(_.group).map { case (g, lst) =>
    s"[$g]\n  " + lst.map(f => s"${f.title} [ID: ${f.id.getOrElse("?")}] (${f._type}${if(f.tags.nonEmpty) " - Tags: " + f.tags.mkString(", ") else ""})${f.containedAs.map(c => s" - contained as: ${c.title} [ID: ${c.id.getOrElse("?")}]").getOrElse("")}${f.from.map(fr => s" - from: ${fr.title} [ID: ${fr.id.getOrElse("?")}]").getOrElse("")}").mkString("\n  ")
  }.mkString("\n  ")}
""")
    }
    meta
  } else None
).seq.groupBy(_.id).mapValues(_.head).toMap

def parseKestraAuthorMeta(id: Int, elem: Element): Option[KestraAuthorMeta] = {
  val h1 = elem.select("h1").headOption
  if (h1.isEmpty || h1.get.text.contains("Page Not Found")) return None
  
  val entityType = elem.select("img.great_symbol").headOption.map(_.attr("alt")).getOrElse("Unknown")

  val fullText = h1.get.text
  val subtitleText = h1.get.select("span.subtitle").headOption.map(_.text).getOrElse("")
  val nameRealName = if (subtitleText.nonEmpty) fullText.replace(subtitleText, "").trim else fullText.trim
    
  val nameMatch = "(.*?)(?:\\((.*?)\\))?$".r.findFirstMatchIn(nameRealName)
  val name = nameMatch.map(_.group(1).trim).getOrElse(nameRealName)
  val altNameFromH1 = nameMatch.flatMap(m => Option(m.group(2)).map(_.trim)).filter(_.nonEmpty)

  val roles = subtitleText.split(",").map(_.trim).filter(_.nonEmpty).toSeq

  def parseYearSpan(text: String): (Option[Int], Option[Int]) = {
    val inMatch = """\s*in (\d{4})-(\d{4})\s*""".r.findFirstMatchIn(text)
    if (inMatch.isDefined) return (Some(inMatch.get.group(1).toInt), Some(inMatch.get.group(2).toInt))
    val singleMatch = """\s*in (\d{4})\s*""".r.findFirstMatchIn(text)
    if (singleMatch.isDefined) return (Some(singleMatch.get.group(1).toInt), Some(singleMatch.get.group(1).toInt))
    val sinceMatch = """\s*since (\d{4})\s*""".r.findFirstMatchIn(text)
    if (sinceMatch.isDefined) return (Some(sinceMatch.get.group(1).toInt), None)
    val untilMatch = """\s*until (\d{4})\s*""".r.findFirstMatchIn(text)
    if (untilMatch.isDefined) return (None, Some(untilMatch.get.group(1).toInt))
    (None, None)
  }

  var activeStart: Option[Int] = None
  var activeEnd: Option[Int] = None
  var country: Option[String] = None
  val aliases = Buffer[KestraAlias]()
  val groups = Buffer[KestraAuthorRef]()
  val exSysop = Buffer[KestraAuthorRef]()
  val alsoListed = Buffer[KestraAuthorRef]()
  var phone: Option[String] = None
  val memberCountries = Buffer[String]()
  val organizedParties = Buffer[KestraAuthorRef]()
  val publishedSeries = Buffer[KestraAuthorRef]()

  var realName: Option[String] = None
  var altName: Option[String] = altNameFromH1

  elem.select("ul.nodots li").foreach { li =>
    val text = li.text
    if (text.startsWith("Known as active ")) {
      val spanInfo = parseYearSpan(text.replace("Known as active ", "in ").trim)
      activeStart = spanInfo._1
      activeEnd = spanInfo._2
    } else if (text.startsWith("Country: ")) {
      country = Some(text.replace("Country:", "").trim)
    } else if (text.startsWith("Known as: ")) {
      val als = text.replace("Known as: ", "").split(",").map(_.trim)
      als.foreach { a =>
        val spanMatch = """\((in|since|until)\s+(\d{4}.*?)\)""".r.findFirstMatchIn(a)
        val spanInfo = spanMatch.map(m => parseYearSpan(s"${m.group(1)} ${m.group(2)}")).getOrElse((None, None))
        val cleanedAlias = a.replaceAll("""\((in|since|until)\s+\d{4}.*?\)""", "").trim
        if (cleanedAlias.endsWith("(realname)")) {
          val rNameStr = cleanedAlias.replace("(realname)", "").trim
          val rNameParts = rNameStr.split("\\(")
          realName = Some(rNameParts.head.trim)
          if (rNameParts.length > 1) {
            aliases ++= rNameParts.tail.map(rp => KestraAlias(rp.replace(")", "").trim, None, None))
          }
        } else {
          val aliasNameMatch = "(.*?)(?:\\((.*?)\\))?$".r.findFirstMatchIn(cleanedAlias)
          val finalAlias = aliasNameMatch.map(_.group(1).trim).getOrElse(cleanedAlias)
          if (finalAlias.nonEmpty) aliases += KestraAlias(finalAlias, spanInfo._1, spanInfo._2)
        }
      }
    } else if (text.startsWith("Known as member of ")) {
      val parts = li.innerHtml.split("</a>")
      val aTags = li.select("a").toSeq
      aTags.zipWithIndex.foreach { case (a, idx) =>
        val trailingChunk = if (idx + 1 < parts.length) parts(idx + 1) else ""
        val spanText = trailingChunk.split("<a").headOption.getOrElse("")
        val spanInfo = parseYearSpan(spanText)
          
        val href = a.attr("href")
        val gId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt).getOrElse(0)
        groups += KestraAuthorRef(gId, a.text, spanInfo._1, spanInfo._2)
      }
    } else if (text.startsWith("Ex System Operator of ")) {
      val parts = li.innerHtml.split("</a>")
      val aTags = li.select("a").toSeq
      aTags.zipWithIndex.foreach { case (a, idx) =>
        val trailingChunk = if (idx + 1 < parts.length) parts(idx + 1) else ""
        val spanText = trailingChunk.split("<a").headOption.getOrElse("")
        val spanInfo = parseYearSpan(spanText)
          
        val href = a.attr("href")
        val gId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt).getOrElse(0)
        exSysop += KestraAuthorRef(gId, a.text, spanInfo._1, spanInfo._2)
      }
    } else if (text.startsWith("Also with this name: ")) {
      li.select("a").foreach { a =>
        val href = a.attr("href")
        val gId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt).getOrElse(0)
        alsoListed += KestraAuthorRef(gId, a.text)
      }
    } else if (text.startsWith("Ex BBS Phone Number(s): ") || text.startsWith("BBS Phone Number(s): ")) {
      phone = Some(text.replace("Ex BBS Phone Number(s):", "").replace("BBS Phone Number(s):", "").trim)
    } else if (text.startsWith("Members countries: ")) {
      li.select("img").foreach { img =>
        val title = img.attr("title").trim
        if (title.nonEmpty) memberCountries += title
      }
    } else if (text.startsWith("Organized parties: ")) {
      li.select("a").foreach { a =>
        val href = a.attr("href")
        val gId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt).getOrElse(0)
        organizedParties += KestraAuthorRef(gId, a.text)
      }
    } else if (text.startsWith("Published series: ")) {
      li.select("a").foreach { a =>
        val href = a.attr("href")
        val gId = """id=(\d+)""".r.findFirstMatchIn(href).map(_.group(1).toInt).getOrElse(0)
        publishedSeries += KestraAuthorRef(gId, a.text)
      }
    }
  }

  Some(KestraAuthorMeta(
    id = id,
    name = name,
    altName = altName,
    entityType = entityType,
    realName = realName,
    roles = roles,
    activeStart = activeStart,
    activeEnd = activeEnd,
    country = country,
    aliases = aliases.toSeq,
    groups = groups.toSeq,
    exSysop = exSysop.toSeq,
    alsoListed = alsoListed.toSeq,
    phone = phone,
    memberCountries = memberCountries.toSeq,
    organizedParties = organizedParties.toSeq,
    publishedSeries = publishedSeries.toSeq
  ))
}

val authors = Files.list(Paths.get(kestra_path + "author/")).toScala(Buffer).par.flatMap(f =>
  val doc = JsoupBrowser().parseFile(f.toFile)
  val data = doc >> elementList("#content")
  if (data.length > 0) {
    val id = f.toString().split("=").last.toInt
    val meta = parseKestraAuthorMeta(id, data(0))
    meta.foreach { meta =>
      val startStr = meta.activeStart.map(_.toString).getOrElse("?")
      val endStr = meta.activeEnd.map(_.toString).getOrElse("?")
      val actStr = if (meta.activeStart.isDefined || meta.activeEnd.isDefined) s"$startStr-$endStr" else "N/A"
      
      println(s"""
=== Kestra Author ID: ${meta.id} ===
Name: ${meta.name}
Alt Name: ${meta.altName.getOrElse("N/A")}
Type: ${meta.entityType}
Real Name: ${meta.realName.getOrElse("N/A")}
Roles: ${if (meta.roles.nonEmpty) meta.roles.mkString(", ") else "N/A"}
Active Span: $actStr
Country: ${meta.country.getOrElse("N/A")}
Aliases: ${if (meta.aliases.nonEmpty) meta.aliases.map(a => {
  val aStart = a.activeStart.map(_.toString).getOrElse("?")
  val aEnd = a.activeEnd.map(_.toString).getOrElse("?")
  val aAct = if(a.activeStart.isDefined || a.activeEnd.isDefined) s" [span: $aStart-$aEnd]" else ""
  s"${a.name}$aAct"
}).mkString(", ") else "None"}
Groups: ${if (meta.groups.nonEmpty) meta.groups.map { g =>
  val gStart = g.activeStart.map(_.toString).getOrElse("?")
  val gEnd = g.activeEnd.map(_.toString).getOrElse("?")
  val gAct = if(g.activeStart.isDefined || g.activeEnd.isDefined) s" [span: $gStart-$gEnd]" else ""
  s"${g.name}$gAct [ID: ${g.id}]"
}.mkString(", ") else "None"}
Ex-Sysop: ${if (meta.exSysop.nonEmpty) meta.exSysop.map { g =>
  val gStart = g.activeStart.map(_.toString).getOrElse("?")
  val gEnd = g.activeEnd.map(_.toString).getOrElse("?")
  val gAct = if(g.activeStart.isDefined || g.activeEnd.isDefined) s" [span: $gStart-$gEnd]" else ""
  s"${g.name}$gAct [ID: ${g.id}]"
}.mkString(", ") else "None"}
Also Listed: ${if (meta.alsoListed.nonEmpty) meta.alsoListed.map(g => s"${g.name} [ID: ${g.id}]").mkString(", ") else "None"}
Phone: ${meta.phone.getOrElse("N/A")}
Member Countries: ${if (meta.memberCountries.nonEmpty) meta.memberCountries.mkString(", ") else "None"}
Organized Parties: ${if (meta.organizedParties.nonEmpty) meta.organizedParties.map(p => s"${p.name} [ID: ${p.id}]").mkString(", ") else "None"}
Published Series: ${if (meta.publishedSeries.nonEmpty) meta.publishedSeries.map(s => s"${s.name} [ID: ${s.id}]").mkString(", ") else "None"}
""")
    }
    meta
  } else None
).groupBy(_.id).seq

val composer_handles = authors.values.par.flatMap(_.filter(a => a.realName.isDefined && a.entityType == "Author" && a.roles.contains("Music Artist") && a.realName.get.contains(" ")).flatMap(author =>
  val realName = normalizeRealName(author.realName.get.trim, author.name.trim).getOrElse("")
  if (realName.nonEmpty) {
    val handle =
      if (author.name.trim.nonEmpty && !author.name.trim.equalsIgnoreCase(realName)) author.name.trim
      else author.aliases.sortBy(a => (a.activeStart.getOrElse(9999), a.activeEnd.getOrElse(0))).reverse.map(_.name.trim).distinct.filterNot(a => a.isEmpty || a.equalsIgnoreCase(realName)).headOption.getOrElse("")
    if (handle.nonEmpty) Some(realName -> handle)
    else None
  } else None
)).seq.toMap

val all_aliases = authors.values.flatMap(_.filter(a => a.entityType == "Author" && a.roles.contains("Music Artist")).flatMap(author =>
  val handle = author.name.trim
  val realName = author.realName.map(_.trim).getOrElse("")
  val aliases = author.aliases.map(_.name.trim).filter(a => a.nonEmpty && !a.equalsIgnoreCase(realName) && !a.equalsIgnoreCase(handle)).distinct
  val altName = author.altName.getOrElse("").trim

  val generatedNames = if (realName.nonEmpty) generateNameVariants(realName) else Iterable.empty[String]

  val rawNames = (Seq(handle) ++ Seq(realName) ++ aliases ++ Seq(altName) ++ generatedNames).filterNot(_.isEmpty).distinct

  if (rawNames.nonEmpty) {
    val normalizedNames = rawNames.map(normalizeAuthor)
    normalizedNames.map(n => n -> rawNames.distinct.toBuffer)
  } else Iterable.empty[(String, Buffer[String])]
)).groupBy(_._1).view.mapValues(_.flatMap(_._2).toBuffer.distinct).toMap

def _date(d: Option[String]) = d.map(_.length).getOrElse(0) match {
  case 4 => d.get + "-99-99"
  case 7 => d.get + "-99"
  case 10 => d.get
  case _ => "9999-99-99"
}

val metas = releases.filter { case (id, meta) =>
  meta.types.contains("Music") &&
  !Set("C64 SID","IFF 8SVX","MPEGA encoded music","WAV").contains(meta.soundFormat.getOrElse(""))
}.par.map { case (id, meta) =>
  def _party(p: String) = p
    .replaceAll(" \\d{4}$", "")
    .replaceAll(" \\d{4} [Aa]utumn$", "")
    .replaceAll(" \\d{4} [Ss]pring$", "")
    .replaceAll(" \\d{4} WE$", "")
    .replaceAll(" '?[8-9][0-9]$", "")
    .replaceAll(" \\(.*\\)$", "")
    .trim
  val authors =
    if (meta.tags.contains("No author infos")) Buffer.empty[String]
    else meta.authors.map(a => a.name.trim).toBuffer.sorted.distinct
  var album = ""
  var publishers = Buffer.empty[String]
  var year = 0
  var _type = ""
  var _platform = ""
  val preferred = Seq("Demo","Game","Intro","Megademo","Trackmo")
  val connections = meta.connections
    .filterNot(_.group == "Evolution Tree")
    .filter(_.id.isDefined)
    .map(c => (c, releases(c.id.get)))
    .sortBy(c => _date(c._2.released) + (if (preferred.intersect(c._2.types).nonEmpty) "-0" else "-1"))
    .filter(c => _date(c._2.released).take(4).toInt <= _date(meta.released).take(4).toInt + 1)
  val mindate = _date(connections.map(c => _date(c._2.released)).minOption)
  val featuredIn = connections.filter(_._1.group == "Featured In")
  var release = featuredIn.headOption.orElse(connections.headOption).map(_._2)
  if (_date(release.flatMap(_.released)) > mindate && connections.find(c => _date(c._2.released) <= mindate).isDefined) {
    val better = connections.find(c => _date(c._2.released) <= mindate).get
    if (preferred.intersect(better._2.types).nonEmpty) {
      release = Some(better._2)
    }
  }
  val origin = meta.origin.flatMap(o => o.id.flatMap(releases.get))
  if (origin.isDefined && (!release.isDefined || _date(origin.get.released) <= _date(release.get.released))) {
    release = Some(origin.get)
  } else if (origin.isDefined && featuredIn.headOption.isDefined && origin.get.id == featuredIn.headOption.get._1.id.get) {
    release = Some(origin.get)
  }
  if (release.isDefined && release.get.types.contains("Music-Exe")) {
    val other =
      origin
        .filterNot(_.types.contains("Music-Exe"))
        .filter(r => _date(r.released) <= _date(release.get.released))
      .orElse(featuredIn
        .map(_._2)
        .filterNot(_.types.contains("Music-Exe"))
        .filter(r => _date(r.released) <= _date(release.get.released))
        .headOption)
      .orElse(connections
        .map(_._2)
        .filterNot(_.types.contains("Music-Exe"))
        .filter(r => _date(r.released) <= _date(release.get.released))
        .headOption)
    if (other.isDefined)
      release = Some(other.get)
  }
  if (meta.madeOrFinished.isDefined && (release.isEmpty || release.get.released.isEmpty || _date(meta.madeOrFinished).take(4).toInt + 1 <= _date(release.get.released).take(4).toInt) && (meta.released.isEmpty || _date(meta.madeOrFinished).take(4).toInt + 1 < _date(meta.released).take(4).toInt)) {
    year = meta.madeOrFinished.get.take(4).toInt
  } else if (meta.released.isDefined && meta.party.isDefined &&
     (release.isEmpty || release.get.released.isEmpty || _date(meta.released) < _date(release.get.released) ||
     (_date(meta.released) <= _date(release.get.released) && Seq("Music-Exe", "Musicdisk").intersect(release.get.types).nonEmpty))) {
    publishers = Buffer(_party(meta.party.get.name))
    year = meta.released.get.take(4).toIntOption.getOrElse(0)
    _type = "Compo"
  } else if (release.isDefined && release.get.types.contains("Music-Exe")) {
    val r = release.get
    year = r.released.map(_.take(4).toIntOption.getOrElse(0)).getOrElse(0)
    if (release.get.party.isDefined) {
      publishers = Buffer(_party(release.get.party.get.name))
      _type = "Compo"
    } else if (r.authors.nonEmpty && r.authors != meta.authors) {
      publishers = r.authors.map(_.name.trim).toBuffer.sorted.distinct
    }
    _platform = "Amiga"
  } else if (release.isDefined) {
    val r =
      if (release.get.origin.isDefined) release.get.origin.flatMap(o => o.id.flatMap(releases.get)).getOrElse(release.get)
      else if (release.get.connections.exists(_.group.matches("^Part [0-9] of .*"))) release.get.connections.find(_.group.matches("^Part [0-9] of .*")).flatMap(r => r.id.flatMap(releases.get)).getOrElse(release.get)
      else release.get
    album = r.title.trim
    publishers = r.authors.map(_.name.trim).toBuffer.sorted.distinct
    // XXX Jurassic Pack 18
    if (r.types.contains("Diskmagazine") && r.authors.size >= 10) {
      publishers = Buffer.empty
    }
    year =
      if (r.released.isDefined) r.released.get.take(4).toIntOption.getOrElse(0)
      else if (r.party.isDefined && r.party.get.name.matches(".*\\d{4}$")) r.party.get.name.takeRight(4).toIntOption.getOrElse(0)
      else if (meta.released.isDefined) meta.released.get.take(4).toIntOption.getOrElse(0)
      else 0
    _type = r.types.headOption.getOrElse("")
    _platform =  "Amiga"
  } else if (meta.released.isDefined) {
    year = meta.released.get.take(4).toIntOption.getOrElse(0)
  }
  val md5s = meta.downloads.filterNot(d =>
    (meta.downloads.size > 1 && d.filename.toLowerCase.startsWith("smpl.")) ||
    (meta.downloads.size > 1 && d.filename.toLowerCase.startsWith("smp.")) ||
    (meta.downloads.size > 1 && d.filename.toLowerCase.endsWith(".smpl")) ||
    (meta.soundFormat.getOrElse("") == "Unknown Soundformat" && d.filename.toLowerCase.endsWith(".aiff")) ||
    (meta.soundFormat.getOrElse("") == "ADPCM/Streamed" && d.filename.toLowerCase.endsWith(".wav")) ||
    ((meta.soundFormat.getOrElse("") == "Audio Sculpture" || meta.soundFormat.getOrElse("") == "Startrekker AM/FM")&& (d.filename.toLowerCase.endsWith(".mod.nt") || d.filename.toLowerCase.endsWith(".mod.as"))) ||
    d.fileType.getOrElse("") == "exec" || d.fileType.getOrElse("") == "disk"
  ).flatMap(d =>
    val md5 = d.md5.getOrElse("").toLowerCase
    val crc32 = d.crc.map(_.toLowerCase).getOrElse("")
    val filesize = d.filesize.getOrElse(0)
    val url = URLDecoder.decode(
      // XXX https://amp.dascene.net/modules/T/The%%20Hooligan/MOD.TKT.gz
      d.url.replace("%%20", "%20"),
      "UTF-8"
    )
    val _md5s = Buffer.empty[String]
    if (crc32.nonEmpty && sources.by_crc32_filesize.contains((crc32, filesize))) {
      val candidates = sources.by_crc32_filesize((crc32, filesize))
      if (candidates.size >= 1) {
        assert(candidates.forall(_.md5 == candidates.head.md5))
        _md5s += candidates.head.md5
      }
    } else if (crc32.nonEmpty) {
      println(s"DEBUG: CRC32+filesize not found for URL: ${d.url} (parsed crc32: ${crc32}, filesize: ${filesize}) meta: ${meta}")
    }
    
    def filesizeOk(e: sources.SourceDBEntry): Boolean = {
      if (filesize == 0) false else Math.abs(e.filesize - filesize) / filesize.toDouble <= 0.00025
    }
    val metaSonglength = parsePlayingTime(meta.playingTime.getOrElse("0")) * 1000
    def songlengthOk(e: sources.SourceDBEntry): Boolean = {
      if (meta.playingTime.isEmpty) false else {
        math.abs(songlengths.songlengthsByMd5(e.md5.take(12)).head.subsongs.head.songlength - metaSonglength) <= 3500
      }
    }

    def matchPath(pathMap: collection.Map[String, Seq[sources.SourceDBEntry]], siteLabel: String, altFilename: String, unknownAuthorIndicator: String, path: String): Seq[String] = {
      val found = Buffer.empty[String]
      if (pathMap.contains(path)) {
        found += pathMap(path).head.md5
        return found.toSeq.sorted.distinct
      }

      val filename = Try(URLDecoder.decode(d.filename, "UTF-8")).getOrElse(d.filename).toLowerCase
      val by_filename = pathMap.keys.filter(_.matches(s".*/(?:${Pattern.quote(filename)}|[^/]*[^a-z0-9]${Pattern.quote(altFilename)})$$")).flatMap(pathMap)
      var author_path = path.split("/").dropRight(1).lastOption.getOrElse("___...___")
      var by_author = pathMap.keys.filter(_.contains(s"/${author_path}/")).flatMap(pathMap)
      var by_filename_authors = by_filename.toSeq.intersect(by_author.toSeq).distinct

      if ((by_author.isEmpty || (by_filename_authors.isEmpty && author_path == unknownAuthorIndicator)) && meta.authors.size >= 1) {
        author_path = meta.authors.head.name.toLowerCase
        by_author = pathMap.keys.filter(_.contains(s"/${author_path}/")).flatMap(pathMap)
        by_filename_authors = by_filename.toSeq.intersect(by_author.toSeq).distinct
      }

      lazy val by_filename_filesize = by_filename.filter(filesizeOk)
      lazy val by_filename_crc32 = by_filename.filter(f => crc32.nonEmpty && f.crc32 == crc32.toLowerCase)

      lazy val by_filename_authors_filesize = by_filename_authors.intersect(by_filename_filesize.toSeq).distinct
      lazy val by_filename_authors_crc32 = by_filename_authors.intersect(by_filename_crc32.toSeq).distinct

      lazy val by_filename_authors_songlength = by_filename_authors.filter(e => songlengthOk(e))
      lazy val by_filename_filesize_songlength = by_filename_filesize.filter(e => songlengthOk(e))
      lazy val by_filename_crc32_songlength = by_filename_crc32.filter(e => songlengthOk(e))
      lazy val by_author_filesize_songlength = by_author.filter(e => filesizeOk(e) && songlengthOk(e))
      lazy val by_author_crc32_songlength = by_author.filter(e => crc32.nonEmpty && e.crc32 == crc32.toLowerCase && songlengthOk(e))
      lazy val by_filename_authors_filesize_songlength = by_filename_authors_filesize.filter(songlengthOk)
      lazy val by_filename_authors_crc32_songlength = by_filename_authors_crc32.filter(songlengthOk)
      lazy val by_author_filesize = by_author.filter(filesizeOk)
      lazy val by_author_crc32 = by_author.filter(f => crc32.nonEmpty && f.crc32 == crc32.toLowerCase)

      def pickAndLog(cand: Iterable[sources.SourceDBEntry], tag: String): Boolean = {
        if (cand.size >= 1 && cand.forall(_.md5 == cand.head.md5)) {
          println(s"DEBUG: Matched by ${tag} for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${cand}")
          found += cand.head.md5
          true
        } else false
      }

      if (pickAndLog(by_filename_authors_filesize_songlength, "filename+authors+filesize+songlength")) {}
      else if (pickAndLog(by_filename_authors_crc32_songlength, "filename+authors+crc32+songlength")) {}
      else if (pickAndLog(by_filename_authors_filesize, "filename+authors+filesize")) {}
      else if (pickAndLog(by_filename_authors_crc32, "filename+authors+crc32")) {}
      else if (by_filename_authors_songlength.size >= 1 && by_filename_authors_songlength.forall(_.md5 == by_filename_authors_songlength.head.md5) && author_path != unknownAuthorIndicator) {
        println(s"DEBUG: Matched by filename+authors+songlength for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors_songlength}")
        found += by_filename_authors_songlength.head.md5

      } else if (by_author_filesize_songlength.size >= 1 && by_author_filesize_songlength.forall(_.md5 == by_author_filesize_songlength.head.md5) && author_path != unknownAuthorIndicator) {
        println(s"DEBUG: Matched by author+filesize+songlength for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_filesize_songlength}")
        found += by_author_filesize_songlength.head.md5

      } else if (by_author_crc32_songlength.size >= 1 && by_author_crc32_songlength.forall(_.md5 == by_author_crc32_songlength.head.md5)) {
        println(s"DEBUG: Matched by author+crc32+songlength for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_crc32_songlength}")
        found += by_author_crc32_songlength.head.md5

      } else if (pickAndLog(by_filename_filesize_songlength, "filename+filesize+songlength")) {}
      else if (pickAndLog(by_filename_crc32_songlength, "filename+crc32+songlength")) {}
      else if (by_filename_authors.size >= 1 && by_filename_authors.forall(_.md5 == by_filename_authors.head.md5) && author_path != unknownAuthorIndicator) {
        println(s"DEBUG: Matched by filename+authors for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors}")
        found += by_filename_authors.head.md5

      } else if (pickAndLog(by_filename_filesize, "filename+filesize")) {}
      else if (pickAndLog(by_filename_crc32, "filename+crc32")) {}
      else if (by_author_filesize.size >= 1 && by_author_filesize.forall(_.md5 == by_author_filesize.head.md5) && author_path != unknownAuthorIndicator) {
        println(s"DEBUG: Matched by author+filesize for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_filesize}")
        found += by_author_filesize.head.md5

      } else if (pickAndLog(by_author_crc32, "author+crc32")) {}

      if (by_filename_authors_filesize_songlength.size > 1) {
        println(s"WARN: (0) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors_filesize_songlength}")
      } else if (by_filename_authors_crc32_songlength.size > 1) {
        println(s"WARN: (1) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors_crc32_songlength}")
      } else if (by_filename_authors_filesize.size > 1 && author_path != unknownAuthorIndicator) {
        println(s"WARN: (2) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors_filesize}")
      } else if (by_filename_authors_crc32.size > 1) {
        println(s"WARN: (3) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors_crc32}")
      } else if (by_filename_authors_songlength.size > 1) {
        println(s"WARN: (4) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors_songlength}")
      } else if (by_author_filesize_songlength.size > 1) {
        println(s"WARN: (5) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_filesize_songlength}")
      } else if (by_author_crc32_songlength.size > 1) {
        println(s"WARN: (6) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_crc32_songlength}")
      } else if (by_filename_filesize_songlength.size > 1) {
        println(s"WARN: (7) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_filesize_songlength}")
      } else if (by_filename_crc32_songlength.size > 1) {
        println(s"WARN: (8) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_crc32_songlength}")
      } else if (by_filename_authors.size > 1 && author_path != unknownAuthorIndicator) {
        println(s"WARN: (9) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_authors}")
      } else if (by_filename_filesize.size > 1) {
        println(s"WARN: (10) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_filesize}")
      } else if (by_filename_crc32.size > 1) {
        println(s"WARN: (11) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_filename_crc32}")
      } else if (by_author_filesize.size > 1) {
        println(s"WARN: (12) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_filesize}")
      } else if (by_author_crc32.size > 1) {
        println(s"WARN: (13) Multiple ${siteLabel} matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${by_author_crc32}")
      }

      if (found.isEmpty) {
        println(s"DEBUG: No ${siteLabel} match for URL: ${d.url} (parsed path: ${path}) meta: ${meta}")
      }

      found.toSeq.sorted.distinct
    }

    if (_md5s.isEmpty && (url.contains("://amp.dascene.net/modules/") ||
        url.toLowerCase.contains("/modules/0-9/") ||
        url.matches(".*/[Mm]odules/[A-Z]/.*")
    )) {
      val path = url.replaceAll("http[s]?://.*/[Mm]odules//?","").replace("//","/").toLowerCase
      if (sources.amp_by_path.contains(path)) {
        _md5s += sources.amp_by_path(path).head.md5
      } else {
        _md5s ++= matchPath(sources.amp_by_path, "AMP", Try(URLDecoder.decode(d.filename, "UTF-8")).getOrElse(d.filename).toLowerCase.replaceFirst("^[a-z]+\\.", ""), "unknowncomposers", path)
      }
    } else if (_md5s.isEmpty && url.matches("(?i).*://(?:ftp\\.)?modland\\.com/pub/modules/.*")) {
      val path = url.replaceAll("(?i)http[s]?://(?:ftp\\.)?modland\\.com/pub/modules//?","").replace("//","/").toLowerCase
      if (sources.modland_by_path.contains(path)) {
        _md5s += sources.modland_by_path(path).head.md5
      } else {
        _md5s ++= matchPath(sources.modland_by_path, "MODLAND", Try(URLDecoder.decode(d.filename, "UTF-8")).getOrElse(d.filename).toLowerCase, "- unknown", path)
      }
    } else if (_md5s.isEmpty && url.contains("://wt.exotica.org.uk/files/")) {
      val path = url
        .replaceAll("http[s]?://wt.exotica.org.uk/files//?","")
        .replace("//","/")
        .toLowerCase
      if (sources.wantedteam_by_path.contains(path)) {
        val entries = sources.wantedteam_by_path(path)
        if (entries.size > 1) {
          println(s"WARN: Multiple Wanted Team matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${entries}")
        }
        _md5s += sources.wantedteam_by_path(path).head.md5
      } else {
        println(s"WARN: No Wanted Team match for URL: ${d.url} (parsed path: ${path}) meta: ${meta}")
      }
    } else if (_md5s.isEmpty && (
               url.contains("://files.exotica.org.uk/?file=exotica/media/audio/UnExoticA/") ||
               url.contains("://www.exotica.org.uk/tunes/archive/Authors/")
    )) {
      val path = url
        .replaceAll("http[s]?://files.exotica.org.uk/\\?file=exotica/media/audio/UnExoticA//?","")
        .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive/Authors//?","")
        .replace("//","/")
        .toLowerCase
      if (sources.unexotica_by_path.contains(path)) {
        val entries = sources.unexotica_by_path(path)
        if (entries.size > 1) {
          println(s"WARN: Multiple UnExoticA matches for URL: ${d.url} (parsed path: ${path}) meta: ${meta} candidates: ${entries}")
        }
        _md5s += sources.unexotica_by_path(path).head.md5
      } else {
        println(s"WARN: No UnExoticA match for URL: ${d.url} (parsed path: ${path}) meta: ${meta}")
      }
    } else if (_md5s.isEmpty && (
      url.contains("://www.exotica.org.uk/tunes/archive/") ||
      url.contains("://old.exotica.org.uk/tunes/archive/")
    )) {
      val archive = url
        .replaceAll("http[s]?://www.exotica.org.uk/tunes/archive//?","")
        .replaceAll("http[s]?://old.exotica.org.uk/tunes/archive//?","")
        .replace("//","/")
        .toLowerCase
      if (oldexotica.oldexotica_by_archive.contains(archive)) {
        val entries = oldexotica.oldexotica_by_archive(archive)
        if (entries.size > 1) {
          println(s"WARN: Multiple OldExotica matches for URL: ${d.url} (parsed archive: ${archive}) meta: ${meta} candidates: ${entries}")
        }
        _md5s += oldexotica.oldexotica_by_archive(archive).head.md5
        println(s"DEBUG: Matched by OldExotica archive for URL: ${d.url} (parsed archive: ${archive}) meta: ${meta} candidates: ${entries}")
      } else {
        println(s"WARN: No OldExotica match for URL: ${d.url} (parsed archive: ${archive}) meta: ${meta}")
      }
    }

    if (_md5s.isEmpty && md5.nonEmpty && songlengths.songlengthsByMd5.contains(md5.take(12))) {
      _md5s += md5
    } else if (_md5s.isEmpty && md5.nonEmpty) {
      println(s"DEBUG: MD5 not found for URL: ${d.url} (parsed md5: ${md5}) meta: ${meta}")
    }

    if (_md5s.isEmpty) {
      val filename = Try(URLDecoder.decode(d.filename, "UTF-8")).getOrElse(d.filename).toLowerCase
      val candidates = sources.amp_by_path.keys.filter(_.split("/").last == filename).flatMap(sources.amp_by_path) ++ sources.modland_by_path.keys.filter(_.split("/").last == filename).flatMap(sources.modland_by_path)
      if (candidates.size >= 1 && candidates.forall(_.md5 == candidates.head.md5)) {
         _md5s += candidates.head.md5
      }
    }
    if (_md5s.isEmpty) {
      val filename = normalizeFilename(Try(URLDecoder.decode(d.filename, "UTF-8")).getOrElse(d.filename).toLowerCase)
      val candidates = sources.amp_by_path.keys.filter(k => normalizeFilename(k.split("/").last) == filename).flatMap(sources.amp_by_path) ++ sources.modland_by_path.keys.filter(k => normalizeFilename(k.split("/").last) == filename).flatMap(sources.modland_by_path)
      if (candidates.size >= 1 && candidates.forall(_.md5 == candidates.head.md5)) {
         _md5s += candidates.head.md5
      }
    }

    if (_md5s.isEmpty) {
      println(s"WARN: No MD5 found for download: ${d} meta: ${meta}")
    } else if (_md5s.sorted.distinct.size > 1) {
      println(s"DEBUG: Multiple MD5s found for download: ${d} meta: ${meta} md5s: ${_md5s.sorted.distinct}")
      val sl = _md5s.sorted.distinct.map(md5 => songlengths.songlengthsByMd5(md5.take(12)).map(_.subsongs.head.songlength)).distinct
      if (!sl.forall(_ == sl.head)) {
        println(s"WARN: Multiple different songlengths found for download: ${d} meta: ${meta} md5s: ${_md5s.sorted.distinct} songlengths: ${sl}")
      }
    }
    _md5s.sorted.distinct
  )
  md5s.map(md5 => {
    (id, MetaData(
      hash = md5.take(12),
      authors = authors.sorted.distinct,
      publishers = publishers.sorted.distinct,
      album = album.trim,
      year = year,
      _type = _type.trim,
      _platform = _platform.trim
    ))
  })
}.flatten.seq.toSeq.distinct

val typeBlacklist = Set(
  "ASCII art .diz file",
  "Charset",
  "Crack Info",
  "Graphics",
  "Logo",
  "Music",
  "Music-Exe",
  "Part",
  "Party Invitation Text",
  "Party Result Text",
  "Picture"
)
val kestraMetas = releases.filterNot { case (id, meta) =>
  meta.types.forall(typeBlacklist.contains) || meta.tags.contains("no music")
}.par.map { case (id, meta) =>
  var authors = Seq.empty[Seq[String]]
  val prodMusicAuthors = meta.credits.filter(_.role == "Music").map(a => a.name.trim)
  val musicFeatures = meta.features.filter(_._type == "Music").map(f => releases(f.id.get))
  val musicConnections = meta.connections.filter(_._type == "Music").map(c => releases(c.id.get))
  val musicAuthors = musicFeatures.union(musicConnections).distinct
  if (musicAuthors.nonEmpty) {
    authors = musicAuthors.map(_.authors.map(_.name.trim)).filter(_.nonEmpty).distinct
  }
  if (prodMusicAuthors.size <= 2 && musicAuthors.isEmpty) {
    authors = Seq(prodMusicAuthors)
  } else if (prodMusicAuthors.exists(a => a.nonEmpty && !musicAuthors.exists(_.authors.contains(a)))) {
    prodMusicAuthors.filter(a => a.nonEmpty && !musicAuthors.exists(_.authors.contains(a))).foreach(a => {
      authors = (authors :+ Seq(a)).distinct
    })
  }
  var publishers = meta.authors.map(a => a.name.trim).toBuffer.sorted.distinct
  // XXX Jurassic Pack 18
  if (meta.types.contains("Diskmagazine") && meta.authors.size >= 10) {
    publishers = Buffer.empty
  }
  // XXX 
  publishers = publishers.map(_.replace("Zeppelin Platinum / Zeppelin Games", "Zeppelin"))
  val album = meta.title.trim
  val year = meta.released.flatMap(r => r.take(4).toIntOption).getOrElse(0)
  val _type = meta.types.filterNot(typeBlacklist.contains).headOption.getOrElse("").trim
  if (authors.nonEmpty) {
    authors.distinct.map(a => {
      (meta.id, MetaData(
        hash = "",
        authors = a.sorted.distinct.toBuffer,
        publishers = publishers,
        album = album,
        year = year,
        _type = _type,
        _platform = "Amiga"
      ))
    })
  } else {
    Seq((meta.id, MetaData(
      hash = "",
      authors = Buffer.empty[String],
      publishers = publishers,
      album = album,
      year = year,
      _type = _type,
      _platform = "Amiga"
    )))
  }
}.flatten.seq.toSet

val kestraExtras = releases.filterNot { case (id, meta) =>
  meta.types.forall(typeBlacklist.contains) || meta.types.forall(Set("Packdisk").contains)
}.par.map { case (id, meta) =>
  meta.downloads
  .filter(_.group == "Direct Files")
  .flatMap(d =>
    val url = URLDecoder.decode(
      d.url.replace("%%20", "%20"),
      "UTF-8"
    ).toLowerCase
    if (url.contains("://ftp.amigascne.org/pub/amiga/")) {
      val path = url.replaceAll("http[s]?://ftp.amigascne.org/pub/amiga//?","").replace("//","/")
      val md5s = sources.findArchive(path, sources.amigascne_by_path).map(_._1).sorted.distinct
      md5s.distinct.map((_, (meta, md5s.distinct)))
    } else Buffer.empty
  )
}.flatten.seq.groupBy(_._1).mapValues(_.map(_._2)).par.flatMap { case (md5, _metas) =>
  val mindate = _metas.map(m => _date(m._1.released)).min
  val metas = _metas.filter(m => _date(m._1.released) <= mindate).flatMap { case (meta, md5s) =>
    val prodMusicAuthors = meta.credits.filter(_.role == "Music").map(a => a.name.trim).sorted.distinct
    val musicFeatures = meta.features.filter(_._type == "Music").map(f => releases(f.id.get)).union(meta.connections.filter(_._type == "Music").map(c => releases(c.id.get))).distinct
    val musicFeatureAuthors = musicFeatures.flatMap(_.authors.map(_.name.trim)).sorted.distinct
    val allAuthors = prodMusicAuthors.union(musicFeatureAuthors).sorted.distinct
    var authors = Buffer.empty[String]
    if (md5s.size > 1 && md5s.size > musicFeatures.size) {
      println(s"KESTRA EXTRA: multiple MD5s ${md5s} for meta ${meta} with music features ${musicFeatures}, skipping author matching")
    } else if (allAuthors.size <= 2 && (musicFeatures.isEmpty || musicFeatures.forall(f => f.authors.map(_.name.trim).sorted.distinct == musicFeatures.head.authors.map(_.name.trim).sorted.distinct && f.authors.map(_.name.trim).sorted.distinct == prodMusicAuthors))) {
      authors = allAuthors.toBuffer
    }
    var publishers = Buffer.empty[String]
    var album = ""
    var year = 0
    var _type = ""
    val countBefore = musicFeatures.count(f => _date(f.released).take(4) < _date(meta.released).take(4))
    val countAfter = musicFeatures.count(f => _date(f.released).take(4) >= _date(meta.released).take(4))
    val minMusicFeatureDate = if (musicFeatures.isEmpty) "9999-99-99" else musicFeatures.map(f => _date(f.released)).min
    if ((musicFeatures.isEmpty && allAuthors.size <= 2) || ((countAfter >= countBefore || minMusicFeatureDate.take(4).toInt >= _date(meta.released).take(4).toInt - 2) && (allAuthors.size <= 5 || countBefore == 0) && allAuthors.size <= musicFeatures.size + 2)) {
      album = meta.title.trim
      year = meta.released.flatMap(r => r.take(4).toIntOption).getOrElse(0)
      _type = meta.types.filterNot(typeBlacklist.contains).headOption.getOrElse("").trim
    } else {
      //println(s"KESTRA EXTRA: skipping album/publisher/year/type/platform for md5 ${md5} meta ${meta} with music features ${musicFeatures}, allAuthors ${allAuthors.size}, countBefore ${countBefore}, countAfter ${countAfter}, minMusicFeatureDate ${minMusicFeatureDate}")
    }
    if (authors.nonEmpty || publishers.nonEmpty || album.nonEmpty || year > 0) {
      //println(s"KESTRA EXTRA: matched Amiga Scene File for MD5 ${md5} meta: ${meta} prodMusicAuthors: ${prodMusicAuthors} musicFeatures: ${musicFeatures} musicFeatureAuthors: ${musicFeatureAuthors} allAuthors: ${allAuthors} authors: ${authors} publishers: ${publishers} album: ${album} year: ${year} type: ${_type}")
      Some((meta.id, MetaData(
        hash = md5.take(12),
        authors = authors,
        publishers = publishers,
        album = album,
        year = year,
        _type = _type,
        _platform = "Amiga"
      )))
    } else None
  }
  if (metas.isEmpty) {
    println(s"KESTRA EXTRA: no meta for MD5 ${md5} metas: ${_metas}")
    None
  } else {
    val scoredMetas = metas.map(e =>
      (e, (if (e._2._platform.toLowerCase == "amiga") 1 else 0) + (if (e._2._type.toLowerCase == "game") 1 else 0) + (if (e._2.authors.nonEmpty) 1 else 0) + (if (e._2.publishers.nonEmpty) 1 else 0) + (if (e._2.album.nonEmpty) 1 else 0) + (if (e._2.year > 0) 1 else 0))
    )
    val bestscore = scoredMetas.map(_._2).max
    val bestMetasForScore = scoredMetas.filter(_._2 == bestscore).map(_._1).toSeq

    // Fallback sorting for the "best" entry
    val SORT = "\u0001"
    val bestMeta = bestMetasForScore.sortBy(m => ("" +
     (if (m._2._type.isEmpty) SEPARATOR else if (m._2._type.toLowerCase == "game") 0 else 1) + SORT +
     (if (m._2._platform.isEmpty) SEPARATOR else if (m._2._platform.toLowerCase == "amiga") 0 else 1) + SORT +
     (if (m._2.year == 0) 9999 else m._2.year) + SORT +
     (if (m._2.authors.isEmpty) SEPARATOR else (10 - m._2.authors.size) + m._2.authors.mkString(SEPARATOR)) + SORT +
     (if (m._2.album.isEmpty) SEPARATOR else m._2.album) + SORT +
     (if (m._2.publishers.isEmpty) SEPARATOR else (10 - m._2.publishers.size) + m._2.publishers.mkString(SEPARATOR)) + SORT
    )).head

    Some(bestMeta)
  }
}.seq.toBuffer.distinct
