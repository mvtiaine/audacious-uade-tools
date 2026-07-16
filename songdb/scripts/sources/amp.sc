// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0
//> using dep net.ruippeixotog::scala-scraper::3.1.0
//> using dep org.apache.commons:commons-lang3:3.20.0

import java.nio.file.Files
import java.nio.file.Paths
import java.util.regex.Pattern
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

import org.apache.commons.text.WordUtils

import normalization._

val amp_path = System.getProperty("user.home") + "/sources/metadata/amp/"

final case class AMPMod (
  id: Int,
  md5: String,
  path: String,
  filesize: Int,
)

val amp_mods = Files.list(Paths.get(amp_path + "downmod/")).toScala(Buffer).par.map(f =>
  val loc = Using(scala.io.Source.fromFile(f.toFile())(using scala.io.Codec.UTF8))( _.getLines().find(_.startsWith("location:"))).get
  if (loc.isDefined) {
    val url = loc.get.replace("location: ","")
    val path = java.net.URLDecoder.decode(url,"UTF-8").replaceAll("http[s]?://amp.dascene.net/modules/","")
    if (sources.amp_by_path.contains(path.toLowerCase)) {
      val e = sources.amp_by_path(path.toLowerCase).head
      Some(AMPMod(f.toString().split("=").last.toInt, e.md5, path, e.filesize))
    } else None
  } else None
).flatten.seq

final case class AMPMeta (
  md5: String,
  path: String,
  filesize: Int,
  extra_authors: Buffer[(Int, String)],
  album: String,
  _type: String,
)

final case class AMPDetail (
  id: Int,
  handle: String,
  realName: Option[String],
  realNames: Buffer[String],
  country: String,
  exHandles: Buffer[String],
  groups: Buffer[String],
  metas: Buffer[AMPMeta]
)

val amp_mods_by_id = amp_mods.groupBy(_.id)

val seenIds = scala.collection.mutable.Set[Int]()
val _details = Files.list(Paths.get(amp_path + "detail/")).toScala(Buffer).par.map(f =>
  val doc = JsoupBrowser().parseFile(f.toFile)
  val data = doc >> elementList("#result")
  if (data.length > 1) {
    val id = f.toString().split("=").last.toInt
    val foo = data(0) >> elementList("table tbody tr[class^=\"tr\"]")
    //       <td class="descript">Handle: </td>
    //          <td>1in10      </td>
    val handle = (foo >> texts("td.descript:containsWholeText(Handle: ) + td")).flatten.filterNot(_.trim.isEmpty).map(_.trim).headOption.getOrElse("")
    //       <td class="descript">Real&nbsp;Name: </td>
    //          <td>Jari Pitkänen      </td>
    var realNames = (foo >> texts("td.descript:contains(Real Name) + td")).flatten.filterNot(_.trim.isEmpty).map(_.trim).headOption.map(_.split(",").map(_.trim).filterNot(n => n.toLowerCase == "n/a" || n.toLowerCase == "currently not public" || n.toLowerCase == "unknown").toBuffer).getOrElse(Buffer.empty)
    // Split Robert Österbergh (ex. Robert Ling) -> Robert Österbergh, Robert Ling etc.
    realNames = realNames.flatMap(name => {
      if (name.contains(" (ex. ")) {
        val parts = name.split(" \\(ex\\. ")
        if (parts.length == 2) {
          Seq(parts(0).trim, parts(1).replace(")","").trim)
        } else Seq(name)
      } else Seq(name)
    })
    //       <td class="descript">Lived&nbsp;in: </td>
    //          <td><a href="newresult.php?request=country&amp;search=17"><img src="images/flags5/finland.png" alt="Finland" title="Finland" /> </a>      </td>
    // TODO extract country from title attribute
    val country = (foo >> attrs("td.descript:contains(Lived in) + td a img")("title")).flatten.filterNot(_.trim.isEmpty).map(_.trim).headOption.getOrElse("")
    //       <td class="descript">Ex.Handles: </td>
    //          <td>UNI, Uniko, Bb King, Jari Pitkanen, Varia      </td>
    val exHandles = (foo >> texts("td.descript:contains(Ex.Handles) + td")).flatten.filterNot(_.trim.isEmpty).map(_.trim).headOption.map(_.split(",").map(_.trim).filterNot(_.toLowerCase == "n/a").toBuffer).getOrElse(Buffer.empty)
    //       <td class="descript">Was&nbsp;a&nbsp;member&nbsp;of: </td>
    //          <td><a href="newresult.php?request=groupid&amp;search=2704">MFX (FXM - Muleteer Effect)</a>      </td> 
    val groups = (foo >> texts("td.descript:contains(Was a member of) + td a")).flatten.filterNot(_.trim.isEmpty).map(_.trim).toBuffer

    val bar = data(1) >> elementList("table tbody tr[class^=\"tr\"]")
    val ids = bar >> attrs("href")("td a[href^=\"downmod.php\"]")
    val author_ids = bar >> attrs("href")("td a[href^=\"detail.php\"]")
    val author_names = bar >> texts("td a[href^=\"detail.php\"]")
    val authors = author_ids.lazyZip(author_names)
    val metas = ids.lazyZip(authors).filterNot(_._1.isEmpty).flatMap({case (idlink, authors) =>
      val id = idlink.head.trim.split("=").last.toInt
      if (!seenIds.contains(id) && amp_mods_by_id.contains(id)) {
        seenIds += id
        val e = amp_mods_by_id(id).head
        val extra_authors = authors._1.zip(authors._2)
          .map(a => (a._1.trim.split("=").last.toInt, a._2.trim))
          .filterNot(a => Seq("!to be deleted!","Unknown Composers").contains(a._2))
          .toBuffer
        val filename = e.path.split("/").last
        var album =
          if (filename.matches("^\\w+\\.\\([a-zA-Z0-9].*\\).*"))
            filename.split("\\.\\(").last
              .replaceAll("\\).*","")
              .replaceAll("\\(","")
              .replace("_"," ")
              .replaceAll(" \\[DFC\\]$","") // ???
              .replaceAll(" - DFC$","") // ???
              .replaceAll(" DFC$","") // ???
              // XXX
              .replace("Roat Riot 4WD", "Road Riot 4WD")
              .trim
          else ""
        val format = filename.split("\\.").head
        // XXX
        var filtered = if (album.length > 1) album else ""
        filtered = if (filtered.contains(" ") && filtered.replace(" ","").toIntOption.isDefined) "" else filtered
        filtered = if (filtered == format || extra_authors.exists(_._2.contains(filtered))) "" else filtered
        filtered = if (Seq(
          "16 Betha 362","Big Jim","Crim","DJB","Gammis","Impulse","Jape","Liam the Lemming","Logos","Micken","Mystical",
          "SH3","Statix","Tense","The Hooligan","Turtle","WOTW","X-Ball"
        ).exists(a => extra_authors.exists(_._2 == a))) "" else filtered
        filtered = if (Seq("Look Back in","70's").contains(filtered)) "" else filtered
        // assume all album names refer to games
        Some(AMPMeta(e.md5, e.path, e.filesize, extra_authors, filtered, if (filtered.isEmpty) "" else "Game"))
      } else None
    }).toBuffer
    if (metas.nonEmpty || (handle.toLowerCase != "n/a" && realNames.nonEmpty)) {
      Some(AMPDetail(id, handle, realNames.headOption.flatMap(rn => normalizeRealName(rn, handle)), realNames, country, exHandles, groups, metas))
    } else None
  } else Iterable.empty[AMPDetail]
).flatten.distinct.seq

val details_by_id = _details.groupBy(_.id)
private val details_by_realname = _details
  .filterNot(_.handle.toLowerCase == "n/a")
  .filter(d => d.realNames
    .filterNot(_.toLowerCase == d.handle.toLowerCase)
    .filterNot(_.contains("?"))
    .filter(_.contains(" "))  
    .nonEmpty)
  .groupBy(_.realNames.head)

// XXX special cases
val amp_special_cases = Map(
  "Per Almered" -> "Excellence In Art"
)

val composer_handles = _details.flatMap(detail =>
  if (detail.handle.toLowerCase != "n/a") {
    val rns = detail.realNames
      .filterNot(_.toLowerCase == detail.handle.toLowerCase)
      .filterNot(_.contains("?"))
      .filter(_.contains(" "))
    if (rns.nonEmpty && (detail.metas.nonEmpty || details_by_realname(detail.realNames.head).size == 1)) {
      rns.flatMap(rn => {
        normalizeRealName(rn, detail.handle) match {
          case Some(rn) => Some(rn -> detail.handle)
          case None => None
        }
      })
    } else None
  } else Iterable.empty[(String, String)]
).groupBy(_._1)
  .filter { case (_, pairs) => pairs.map(_._2.toLowerCase).distinct.size == 1 }
  .map { case (name, pairs) => name -> pairs.head._2 }
  .toMap
  ++ amp_special_cases

val normalizeHandlePattern = Pattern.compile(" \\[.*\\]$")
val all_aliases: Map[String, Buffer[String]] = {
  // For each composer, collect all normalized aliases, then map each alias to the full set
  val detail_aliases = _details.par.flatMap { detail =>
    val handle = detail.handle.trim
    if (handle.toLowerCase != "n/a" && handle.nonEmpty) {
      val realNames = detail.realNames.map(_.trim).filter(_.nonEmpty)
      val generatedNames = realNames.flatMap(generateNameVariants)

      val rawNames = Seq(handle) ++ 
                     detail.exHandles.map(_.trim).filter(_.nonEmpty).filterNot(_.toLowerCase == "n/a") ++
                     realNames ++ generatedNames

      val validNames = rawNames
        .map(name => normalizeHandlePattern.matcher(name).replaceAll("").trim)
        .filter(_.nonEmpty)
        .distinct
      if (validNames.nonEmpty) {
        val normalizedNames = validNames.map(normalizeAuthor)
        normalizedNames.map(n => n -> validNames.distinct.toBuffer)
      } else Iterable.empty[(String, Buffer[String])]
    } else Iterable.empty[(String, Buffer[String])]
  }.seq

  val special_aliases = amp_special_cases.toSeq.flatMap { case (rn, h) =>
    val names = Buffer(rn, h)
    Seq(normalizeAuthor(rn) -> names, normalizeAuthor(h) -> names)
  }

  (detail_aliases ++ special_aliases)
    .groupBy(_._1)
    .view
    .mapValues(_.flatMap(_._2).toBuffer.distinct)
    .toMap
}

val _metas = _details.flatMap(_.metas).distinct
val _byAlbum: Map[String, Buffer[AMPMeta]] = _metas.filter(_.album.nonEmpty).toBuffer.groupBy(_.album)
val details = _details.par.map(detail =>
  detail.copy(metas = detail.metas.map(meta =>
    var _meta = if (meta.album.nonEmpty) {
      val m = _byAlbum.getOrElse(meta.album, Buffer.empty).filter(_.extra_authors.exists(a => meta.extra_authors.contains(a)))
      val p = meta.path.split("/").last
      val f = p.split("\\.").head
      if (((m.size == 1 && p == s"${f}.(${meta.album}).gz") || Character.isLowerCase(meta.album.charAt(0))) && !Seq(
          "Bigyo","Darryl Sloan","Laura Shigihara","Max","Mike Anderton","Nightshade","Simon Speight","Unison"
        ).exists(a => meta.extra_authors.contains(a))
      ) {
        meta.copy(album = "", _type = "")
      } else meta
    } else meta
    _meta =
      if (!_meta.album.isEmpty && _meta.album.toLowerCase == _meta.album) _meta.copy(album = WordUtils.capitalize(_meta.album))
      else _meta
    _meta
  ).distinct)
).distinct.seq

def transformAuthors(meta: AMPMeta, detail: AMPDetail): Buffer[String] = {
  if (meta._type == "Game") {
    var ok = true
    val authors = meta.extra_authors.map { case (id, a) =>
      if (id == detail.id && detail.realName.isDefined) detail.realName.get
      else if (details_by_id.contains(id) && details_by_id(id).head.realName.isDefined) details_by_id(id).head.realName.get
      else {
        if (!composer_handles.contains(a)) {
          ok = false
        }
        a
      }
    }
    (if (ok) authors else meta.extra_authors.map(_._2)).sorted.distinct.filterNot(_.isEmpty).toBuffer
  } else meta.extra_authors.map(_._2).sorted.distinct.filterNot(_.isEmpty).toBuffer
}
