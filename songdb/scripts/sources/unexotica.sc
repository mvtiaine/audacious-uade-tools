// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2026 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep io.circe::circe-generic::0.14.6
//> using dep io.circe::circe-yaml::1.15.0
//> using dep net.seeseekey:mediawikixml:1.0.3

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using

import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import io.circe.yaml
import io.circe.yaml.parser

import net.seeseekey.mediawikixml.wikipedia.WikiXMLParserFactory

import normalization._

val unexotica_path = System.getProperty("user.home") + "/sources/metadata/unexotica/"
val xmldump_path = unexotica_path + "2026-04-13.xml"

implicit def h[A,B](implicit a: Decoder[A], b: Decoder[B]): Decoder[Either[A,B]] = {
  val l: Decoder[Either[A,B]]= a.map(Left.apply)
  val r: Decoder[Either[A,B]]= b.map(Right.apply)
  l or r
}

type StringOrList = Either[String,List[String]]
type IntOrList = Either[Int,List[Int]]
final case class UnExoticaMeta (
  `type`: String,
  title: Either[String,Int],
  `alternative titles`: Option[StringOrList],
  composer: StringOrList,
  format: StringOrList,
  year: Either[Int,String],
  team: Option[StringOrList],
  publisher: Option[StringOrList],
  group: Option[StringOrList],
  party: Option[String],
  `box scan`: Option[String],
  `hol id`: Option[IntOrList],
  `lemon id`: Option[IntOrList],
  `rip type`: String,
  `ripped by`: StringOrList,
  comments: Option[String],
)
val metas = sources.sourceDB(sources.Source.UnExotica).par.flatMap(e =>
  val txt = unexotica_path + e.path.split("/").take(3).map(_.replace(".lha", ".txt")).mkString("/")

  def parse(file: String) = {
    val yaml = parser.parse(Using(scala.io.Source.fromFile(file)(using scala.io.Codec.ISO8859))(_.mkString).get)
    val meta = yaml
      .leftMap(err => err: Error)
      .flatMap(_.as[UnExoticaMeta])
      .valueOr(throw _)
    Some(e.md5, e.path, e.filesize, meta)
  }
  if (Files.exists(Paths.get(txt))) parse(txt)
  else None
).groupBy(_._1).map({case (md5, metas) =>
  // pick oldest for duplicates
  if (metas.size > 1) {
    System.err.println(s"WARN: removing duplicate UnExotica entries for md5: ${md5} entries: ${metas}")
  }
  val year = metas.map(m => if (m._4.year.fold(_.toString, _.toString) == "Unknown") 9999 else m._4.year.left.get).min
  metas.filter(m => {
    val cmp = if (m._4.year.fold(_.toString, _.toString) == "Unknown") 9999 else m._4.year.left.get
    year == cmp
  }).seq.sortBy(_._2).head // secondarily sort by path for consistency
})
.map(m => {
  // XXX fix some bad metadata
  val meta = m._4
  val title = meta.title.fold(
    title => title,
    title => title.toString
  )
  if (title == "Wing Commander") m.copy(_4 = m._4.copy(year = Left(1992)))
  else if (title == "Operation Wolf") m.copy(_4 = m._4.copy(year = Left(1988)))
  else if (title == "Impérial") m.copy(_4 = m._4.copy(year = Left(1993)))
  else if (title == "Simon the Sorcerer") m.copy(_4 = m._4.copy(year = Left(1993)))
  else if (title == "Soccer Kid") m.copy(_4 = m._4.copy(year = Left(1993)))
  else if (title == "Elvira II - The Jaws of Cerberus") m.copy(_4 = m._4.copy(year = Left(1992)))
  else if (title == "Cardiaxx") m.copy(_4 = m._4.copy(year = Left(1991)))
  else if (title == "Winter Olympics - Lillehammer'94") m.copy(_4 = m._4.copy(year = Left(1993)))
  else if (title == "Psycho Santa") m.copy(_4 = m._4.copy(year = Left(1993)))
  else if (title == "Elvira II - The Jaws of Cerberus") m.copy(_4 = m._4.copy(year = Left(1992)))
  else if (title == "5th Gear") m.copy(_4 = m._4.copy(year = Left(1990)))
  else if (title == "Pinball Illusions") m.copy(_4 = m._4.copy(year = Left(1995)))
  else if (title == "Scorched Tanks") m.copy(_4 = m._4.copy(year = Left(1994)))
  else m
})
.seq.toSeq

// XXX org.xml.sax.SAXParseException; lineNumber: 410190; columnNumber: 33; JAXP00010003: The length of entity "[xml]" is "100 001" that exceeds the "100 000" limit set by "jdk.xml.maxGeneralEntitySizeLimit".
// -Djdk.xml.maxGeneralEntitySizeLimit=0 -Djdk.xml.totalEntitySizeLimit=0
System.setProperty("jdk.xml.maxGeneralEntitySizeLimit", "0")
System.setProperty("jdk.xml.totalEntitySizeLimit", "0")
val xmldump_parser = WikiXMLParserFactory.getParser(xmldump_path)

final case class WikiFileEntry(
  filename: String,
  size: Int,
  var composers: Seq[String],
  var game: String,
  var year: Int,
  var publishers: Seq[String]
)
val wikiTables = scala.collection.mutable.Map[String, Buffer[WikiFileEntry]]()

xmldump_parser.setPageCallback(page => {
  val cats = page.getCategories.asScala
  if (cats.exists(c => c == "Amiga Games" || c == "Amiga Demos")) {
    var currentArchive = ""
    var inTable = false
    var pathStack = scala.collection.mutable.ArrayBuffer.fill(20)("")
    var entries = Buffer[WikiFileEntry]()
    val ArchiveMatch = """^===\s*(.*?\.lha)\s*===.*""".r
    val fMatch = """\{\{f\|(.*?)\}\}""".r

    def finalizeTable(): Unit = {
      if (currentArchive.nonEmpty && entries.nonEmpty) {
        var globalComposers = Seq[String]()
        var globalGame = ""
        var globalYear = 0
        var globalPublishers = Seq[String]()
        
        for (i <- entries) {
          if (i.composers.nonEmpty) globalComposers = i.composers
          if (i.game.nonEmpty) globalGame = i.game
          if (i.year > 0) globalYear = i.year
          if (i.publishers.nonEmpty) globalPublishers = i.publishers
        }
        for (i <- entries) {
          if (i.composers.isEmpty) i.composers = globalComposers
          if (i.game.isEmpty) i.game = globalGame
          if (i.year <= 0) i.year = globalYear
          if (i.publishers.isEmpty) i.publishers = globalPublishers
        }
        wikiTables(currentArchive) = entries
      }
    }

    for (line <- page.getWikiText.linesIterator) {
      line.trim match {
        case ArchiveMatch(arc) =>
          finalizeTable()
          currentArchive = arc
          entries = Buffer[WikiFileEntry]()
          inTable = false
        case l if l.startsWith("{| class=\"filetable\"") =>
          inTable = true
          pathStack = scala.collection.mutable.ArrayBuffer.fill(20)("")
        case l if inTable && l.startsWith("|}") =>
          inTable = false
        case l if inTable && l.startsWith("|") && !l.startsWith("|-") =>
          val cols = l.stripPrefix("|").split("\\|\\|", -1).map(_.trim)
          if (cols.length >= 6) {
            val depth = fMatch.findFirstMatchIn(cols(0)) match {
              case Some(m) => m.group(1).split("\\|").length
              case None => 0
            }
            val rawName = cols(0).replaceAll("""\{\{f\|.*?\}\}""", "").trim
            if (pathStack.length <= depth) pathStack.padToInPlace(depth + 1, "")
            pathStack(depth) = rawName

            val sizeCol = cols(1).replaceAll("""class="r"\|""", "").trim
            if (sizeCol.nonEmpty) {
              val size = sizeCol.toIntOption.getOrElse(0)
              var composers_ = cols(2)
              // XXX
              if (composers_ == "Paul & Jeff McMaster")
                composers_ = "Paul McMaster & Jeff McMaster"
              else if (composers_ == "Steve & Dave Hasler")
                composers_ = "Steve Hasler & Dave Hasler"
              else if (composers_ == "Jeroen & Michiel Soede")
                composers_ = "Jeroen Soede & Michiel Soede"
              else if (composers_ == "Tim & Geoff Follin")
                composers_ = "Tim Follin & Geoff Follin"
              else if (composers_ == "Tommy/Avena")
                composers_ = "Tommy"
              else if (composers_ == "Stefan Jaworski - Nightlight")
                composers_ = "Nightlight"
              val composers = if (composers_.endsWith(" & Co."))
                Array(composers_)
              else
                composers_.split("&|,|/| or | and ").map(_.trim).filter(c => c.nonEmpty && c != "-").sorted.distinct
              val game = if (cols(3) == "-") "" else cols(3)
              val year = cols(4).toIntOption.getOrElse(0)
              val publishers = cols(5).split("/").map(_.trim).filter(p => p.nonEmpty && p != "-").sorted.distinct
              val path = pathStack.take(depth + 1).filter(_.nonEmpty).mkString("/").trim
              entries.append(WikiFileEntry(path, size, composers, game, year, publishers))
            }
          }
        case _ =>
      }
    }
    finalizeTable()
  }
})
xmldump_parser.parse()

final case class ComposerHandle(
  name: String,
  handle: Option[Either[String,Int]]
)
val composerfiles1 = Files.list(Paths.get(unexotica_path)).toScala(Buffer)
  .filter(p => Seq("Demo", "Game").contains(p.toFile.getName))
  .flatMap(dir => Files.list(dir).toScala(Buffer)
    .filter(_.toFile.isDirectory)
    .flatMap(author => Files.list(author).toScala(Buffer)
      .map(_.toFile)
      .filter(_.isFile)
      .filter(_.getName == "composer.txt")))
val composerfiles2 = Files.list(Paths.get(unexotica_path + "/Demo/Composers/")).toScala(Buffer)
  .map(_.toFile)
  .filter(_.isFile)
  .filter(f => f.getName.endsWith(".txt"))
val composerfiles3 = Files.list(Paths.get(unexotica_path + "/Game/Composers/")).toScala(Buffer)
  .map(_.toFile)
  .filter(_.isFile)
  .filter(f => f.getName.endsWith(".txt"))

val composer_handles = (composerfiles1 ++ composerfiles2 ++ composerfiles3)
  .par
  .flatMap(file => {
    try {
      val yaml = parser.parse(Using(scala.io.Source.fromFile(file)(using scala.io.Codec.ISO8859))(_.mkString).get)
      val meta = yaml
        .leftMap(err => err: Error)
        .flatMap(_.as[ComposerHandle])
        .valueOr(throw _)
      if (meta.handle.nonEmpty) {
        var handle = meta.handle.get.fold(_.toString, _.toString).split(",")(0).trim
        if (handle == "TDK (The Dark Knight) / Madfiddler") {
          handle = "TDK"
        } else if (handle == "Jester Brothers International / Cold Storage") {
          handle = "Jester Brothers International"
        } else if (handle == "Shade/Offworld") {
          handle = "Shade"
        }
        var name = meta.name.replace(" _", " ")
        // XXX
        if (name == "Øisten Eide") {
          name = "Øistein Eide"
        }
        Some(ComposerHandle(name, Some(Left(handle))))
      } else None
    } catch {
      case e: Throwable =>
        System.err.println(s"ERROR processing UNEXOTICA composer file: ${file} error: ${e.getMessage}")
        e.printStackTrace()
        System.exit(1)
        None
    }
  }).groupBy(_.name).map({case (name, metas) =>
    name -> metas.head.handle.get.fold(_.toString, _.toString)
  }).seq.toMap

val all_aliases: Map[String, Buffer[String]] = {
  // For each composer, collect all normalized aliases, then map each alias to the full set
  composer_handles.par.flatMap { case (name, handle) =>
    val rawNames = Seq(handle, name) ++ generateNameVariants(name)
    val validNames = rawNames.filter(_.nonEmpty).distinct
    val normalizedNames = validNames.map(normalizeAuthor)
    normalizedNames.map(n => n -> validNames.distinct.toBuffer)
  }.seq.groupBy(_._1).view.mapValues(_.flatMap(_._2).toBuffer.distinct).toMap
}

val by_path = sources.sourceDB(sources.Source.UnExotica).groupBy(_.path.split("/").take(3).mkString("/"))
def transformAuthors(meta: UnExoticaMeta, path: String): Seq[String] = {
  val handleBlackList = Seq("Allister Brimble", "Tim Wright")
  // XXX Wiki typos etc.
  val fixes = Map(
    "Andrew Cummings" -> "Adrian Cummings",
    "B. Johnston" -> "Brian Johnston",
    "Benn Daglish" -> "Ben Daglish",
    "Charles Deenan" -> "Charles Deenen",
    "Chrisitan Blaha" -> "Christian Blaha",
    "Christian Fruergaard" -> "Christian Fruergård",
    "D. Whittaker" -> "David Whittaker",
    "D. Winderlich" -> "Dag Winderlich",
    "Even Salies" -> "Evens Salies",
    "Frederic Motte" -> "Frédéric Motte",
    "G Assenmacher" -> "G. Assenmacher",
    "Hans-Herman Franck" -> "Hans-Hermann Franck",
    "George Wilkon" -> "George Wilkins",
    "Jochen Feldkoetter" -> "Jochen Feldkötter",
    "Jogier Liljedahl" -> "Jogeir Liljedahl",
    "Lyndon Sharpn" -> "Lyndon Sharp",
    "Marc Francois" -> "Marc François",
    "Martin Silbernagle" -> "Martin Silbernagl",
    "Matthews Simmonds" -> "Matthew Simmonds",
    "Michael Knaep" -> "Michael Knaepen",
    "Paal Granum" -> "Pål Granum",
    "R. Usher" -> "Raymond Usher",
    "Richard Jospeh" -> "Richard Joseph",
    "Ronald Weeserik Pieket" -> "Ronald Pieket Weeserik",
    "Spiny Normal" -> "Spiny Norman",
    "Timm Engles" -> "Timm Engels",
    "Mixed-Up Mother Goose" -> "James Elliot",
    "Øisten Eide" -> "Øistein Eide",
    "Rob Wells" -> "Robert Wells",
  )
  def normalize(name: String, path: String): String = {
    var normalized = name.replaceAll(" \\(.*\\)$", "").trim
    normalized = fixes.get(normalized).getOrElse(normalized)
    normalized = if (path.startsWith("Demo/") && !handleBlackList.contains(normalized)) {
      if (composer_handles.contains(normalized)) composer_handles(normalized)
      else if (amp.composer_handles.contains(normalized)) amp.composer_handles(normalized)
      else normalized
    } else normalized
    // XXX
    if (normalized.startsWith("Unknown") || normalized.endsWith("?") || normalized == "Zylon of AFL") ""
    else normalized
  }
  val prefix = path.split("/").take(3).mkString("/").trim
  var suffix = path.split("/").drop(3).mkString("/").trim
  
  // Quick hack for Glücksrad unicode normalization mismatch between APFS (NFD) and Wiki (NFC)
  suffix = suffix.replace("u\u0308", "\u00fc")
  
  val wikiFiles = wikiTables.get(prefix).map(_.filter(_.filename == suffix)).getOrElse(Seq.empty)
  val wikiComposers = wikiFiles.headOption.map(_.composers).getOrElse(Seq.empty)
  val composers = wikiComposers.map(c => normalize(c, path)).filter(_.nonEmpty).distinct.sorted
  if (composers.size == 1 && (composers.head.endsWith(" et al") || composers.head.endsWith(" & Co."))) {
    // XXX
    if (meta.title == Left("Tales from Heaven") && path.endsWith("p60.game_end")) {
      Seq("Manfred Linzner")
    } else {
      meta.composer match {
        case Right(composers) =>
          val filtered = composers
            .filterNot(_.toLowerCase.endsWith("(sound driver)"))
            .filterNot(_.toLowerCase.endsWith("(original composer)"))
          filtered.map(c => normalize(c, path))
        case Left(composer) =>
          Seq(normalize(composer, path))
      }
    }
  } else composers
}

def transformAlbum(meta: UnExoticaMeta, path: String): String = {
  val authorAlbum = path.substring(path.indexOf("/") + 1, path.lastIndexOf("/")).split("/")
  var title = meta.title.fold(
    title => title,
    title => title.toString
  // remove subtitle parts to try avoid overly long titles
  )
  val short = title.split(" - ").head.trim
  if (!short.toIntOption.isDefined && !title.trim.takeRight(2).toIntOption.isDefined && (short.length > 3 || title.toLowerCase.endsWith(" game")) && !title.toLowerCase.startsWith("the games")) title = short
  if (title.isEmpty) if (authorAlbum.size > 1) authorAlbum(1) else ""
  else title
}

def transformPublishers(meta: UnExoticaMeta): Buffer[String] = {
  val publishers = Buffer.empty[String]

  if (meta.group.isDefined) meta.group.get match {
    case Left(group) => publishers.append(group)
    case Right(groups) => publishers.appendAll(groups)
  }
  if (meta.publisher.isDefined) meta.publisher.get match {
    case Left(publisher) => publishers.append(publisher)
    case Right(publishers_) => publishers.appendAll(publishers_)
  }
  if (meta.team.isDefined) meta.team.get match {
    case Left(team) => publishers.append(team)
    case Right(teams) => publishers.appendAll(teams)
  }

  publishers.map(_.trim)
    .filterNot(_ == "Public Domain")
    .filterNot(_ == "Martin Rebas")
    .sorted.distinct
}
