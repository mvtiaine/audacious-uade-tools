// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>
// mostly vibe coded with Claude 4.5

// NOTE: currently not used due to too unreliable metadata in TOSEC Music

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.util.Using
import scala.util.boundary, boundary.break

import convert._

val allParenthesesPattern = raw"\(([^)]+)\)".r
val composerPattern = raw"\[(cmp [^\]]+)\]|\((cmp [^)]+)\)".r  // matches "[cmp X]" or "(cmp X)"
val sequencerPattern = raw"\[seq ([^\]]+)\]".r
val combinedComposerSequencerPattern = raw"\[cmp ([^-]+)-\s*seq ([^\]]+)\]".r  // matches "[cmp X- seq Y]"
val albumPattern = raw"([^/]+?) - ".r  // match everything before " - " in filename
val albumNoSeparatorPattern = raw"([^/]+?)\s*\(".r  // match everything before "(" if no " - "
val authorOfGroupPattern = raw"^(.+?) of (.+)$$".r
val multiAuthorOfGroupPattern = raw"^(.+?)\s+of\s+(.+)$$".r  // match "Author1 + Author2 of Group"
val yearPattern = raw"^(\d{4})".r  // extract year from beginning of string (handles dates like 1989-08-26)
val validYearPattern = raw"^\d{4}$$".r  // valid year is exactly 4 digits, no 'x' allowed
val articlePattern = raw"^(.+?),\s*(The|A|An|Les?|La|L'|Der)$$".r  // match "Title, The/A/An/Les/Le/La/L'" patterns
val yearOrPlaceholderPattern = raw"^(\d{4}|\d{2}xx|\d{3}x)".r  // matches "1991" or "19xx" or "199x"
val platformPattern = raw"^(AGA|ECS|OCS|CD32|CDTV)$$".r  // platform/technical descriptors to ignore

def splitAuthors(authorString: String): Seq[String] = {
  if (authorString == "Full-FX") {
    return Seq("Full-FX")
  }
  // Remove "by" prefix if present
  val cleaned = authorString.replaceFirst("^by\\s+", "")
  
  // Check for pattern "FirstName1 and FirstName2 LastName" (shared last name)
  val sharedLastNamePattern = raw"^(.+?)\s+and\s+(\S+)\s+(\S+)$$".r
  cleaned match {
    case sharedLastNamePattern(firstName1, firstName2, lastName) =>
      // Split into separate full names with shared last name
      return Seq(s"$firstName1 $lastName", s"$firstName2 $lastName")
    case _ =>
  }
  
  // First split on +, &, or " and " with optional spaces
  val parts = cleaned.split("\\s*[+&]\\s*|\\s+and\\s+").toSeq.flatMap { part =>
    // For each part, check if it contains a hyphen pattern like "Name1 Name2-Name3 Name4"
    // where the hyphen connects two complete names (word before and after are separated by spaces)
    if (part.matches(".*\\s\\S+-\\S+\\s.*")) {
      // Split on hyphen only when it's between space-separated words
      part.split("(?<=\\s\\S+)-(?=\\S+\\s)").map(_.trim).toSeq
    } 
    // Check if it looks like a hyphenated first name followed by a last name (space after hyphenated part)
    // e.g., "Jean-Francois Freites" has a space after the hyphenated part
    else if (part.matches(".*-.*\\s+.*")) {
      // Contains hyphen and space after it, likely "FirstName-MiddleName LastName"
      Seq(part.trim)
    }
    // Otherwise, if it has a hyphen without spaces, split on it (demoscene handles like "Chorus-Sid")
    else if (part.contains("-")) {
      part.split("-").map(_.trim).toSeq
    }
    else {
      Seq(part.trim)
    }
  }
  parts.filter(_.nonEmpty)
}

def splitPublishers(publisherString: String): Seq[String] = {
  // Handle "Share and Enjoy" as a special case - it's a single publisher name
  if (publisherString == "Share and Enjoy") {
    return Seq("Share and Enjoy")
  }
  // Split on "and", "&", or " - " with spaces
  publisherString.split("\\s+(?:and|&)\\s+|\\s+-\\s+").map(_.trim).filter(_.nonEmpty)
}

def normalizeAlbumName(name: String): String = {
  name match {
    case articlePattern(title, article) => s"$article $title"
    case "deathtrap" => "Death Trap"
    case _ => name
  }
}

case class TosecMeta(
  authors: Buffer[String], // [seq X] or [cmp X] or (X)
  composers: Buffer[String], // [cmp X] (with seq X)
  publishers: Buffer[String],
  album: String,
  year: Int
)

def parseTosecMeta(hash: String, path: String): Option[TosecMeta] = {
  val authors = Buffer.empty[String]
  val composers = Buffer.empty[String]
  val publishers = Buffer.empty[String]
  var album = ""
  var year = 0
  
  // Ignore covers and other unreliable metadata
  val filename = path.split("/").last
  if (path.startsWith("Games - AON/") ||
      path.startsWith("Games - BP/") ||
      path.startsWith("Games - FRED/") ||
      path.startsWith("Games - MED/") ||
      path.startsWith("Games - MOD/") ||
      path.startsWith("Music - Games - ML") ||
      path.startsWith("Music - Games - OKT/") ||
      filename.startsWith("ZZZ")) {
    return None
  }

  val isSceneMusic = path.startsWith("Music - Scene")
  val isCovers = path.startsWith("Music - Games - Covers/")
  
  // Extract all parentheses content
  val allMatches = allParenthesesPattern.findAllMatchIn(path).map(_.group(1)).toList
  
  // First match that starts with valid 4-digit year is the year
  // Skip "19xx" style placeholders by checking if the value contains 'x' or 'X'
  // Skip year extraction for covers
  if (!isCovers) {
    allMatches.find { value =>
      yearPattern.findFirstMatchIn(value).isDefined && 
      !value.toLowerCase.contains('x')
    }.foreach { dateStr =>
      yearPattern.findFirstMatchIn(dateStr).foreach { m =>
        year = m.group(1).toInt
      }
    }
  }
  
  // Other matches are publishers/authors
  // Skip both valid years AND year placeholders (19xx, 20xx, etc)
  // Skip publisher extraction for covers
  // Skip "(cmp ...)" entries as they are handled separately
  var hasParenthesesPublisher = false
  if (!isCovers) {
    val nonYearMatches = allMatches.filterNot { value =>
      yearOrPlaceholderPattern.findFirstMatchIn(value).isDefined ||
      value.startsWith("cmp ")  // Filter out composer entries
    }
    
    // Check if we have a publisher already from parentheses (simple name, not "X of Y" pattern)
    hasParenthesesPublisher = nonYearMatches.exists { value =>
      value != "-" && !authorOfGroupPattern.matches(value) && !multiAuthorOfGroupPattern.matches(value) && !platformPattern.matches(value)
    }
    
    nonYearMatches.foreach { value =>
      value match {
        case multiAuthorOfGroupPattern(authorStr, group) =>
          // Split multiple authors and add them
          splitAuthors(authorStr).foreach(authors += _)
          publishers += group
        case authorOfGroupPattern(author, group) =>
          authors += author
          publishers += group
        case "-" =>
          // ignore
        case _ if platformPattern.matches(value) =>
          // ignore platform descriptors
        case _ =>
          if (isSceneMusic) {
            // For scene music, these are authors
            splitAuthors(value).foreach(authors += _)
          } else {
            // Split publishers on "and"
            splitPublishers(value).foreach(publishers += _)
          }
      }
    }
  }
  
  // Extract combined composer-sequencer pattern first
  combinedComposerSequencerPattern.findFirstMatchIn(path).foreach { m =>
    val composerStr = m.group(1).trim
    val sequencerStr = m.group(2).trim
    
    // Remove "of [group]" suffix from composer names when we already have a publisher
    // For covers, never strip the suffix since we don't process publishers
    val cleanedComposer = if (!isCovers && hasParenthesesPublisher) {
      composerStr.replaceFirst("\\s+of\\s+.+$", "")
    } else {
      composerStr
    }
    composers ++= splitAuthors(cleanedComposer)
    
    // Remove "of [group]" suffix from sequencer names when we already have a publisher
    val cleanedSequencer = if (!isCovers && hasParenthesesPublisher) {
      sequencerStr.replaceFirst("\\s+of\\s+.+$", "")
    } else {
      sequencerStr
    }
    splitAuthors(cleanedSequencer).reverse.foreach(authors.prepend(_))
  }
  
  // Extract composers and split multiple names (only if not already processed by combined pattern)
  if (composers.isEmpty) {
    composerPattern.findAllMatchIn(path).foreach { m =>
      // Try group(1) first (square brackets), then group(2) (round brackets)
      val composerStr = Option(m.group(1)).orElse(Option(m.group(2))).getOrElse("")
        .stripPrefix("cmp ").trim
      // Remove "of [group]" suffix from composer names when we already have a publisher
      // For covers, never strip the suffix since we don't process publishers
      val cleanedComposer = if (!isCovers && hasParenthesesPublisher) {
        composerStr.replaceFirst("\\s+of\\s+.+$", "")
      } else {
        composerStr
      }
      composers ++= splitAuthors(cleanedComposer)
    }
  }
  
  // Extract sequencer (if present, add to authors) and split multiple names (only if not already processed)
  if (authors.isEmpty) {
    sequencerPattern.findFirstMatchIn(path).foreach { m =>
      val sequencerStr = m.group(1)
      // Remove "of [group]" suffix from sequencer names when we already have a publisher
      // For covers, never strip the suffix since we don't process publishers
      val cleanedSequencer = if (!isCovers && hasParenthesesPublisher) {
        sequencerStr.replaceFirst("\\s+of\\s+.+$", "")
      } else {
        sequencerStr
      }
      splitAuthors(cleanedSequencer).reverse.foreach(authors.prepend(_))
    }
  }
  
  // Extract album - skip for "Music - Scene -" paths and covers
  if (!isSceneMusic && !isCovers) {
    albumPattern.findFirstMatchIn(filename) match {
      case Some(m) => 
        val rawAlbum = m.group(1).trim
        // Album is only the part before first ( or [
        val cleanAlbum = rawAlbum.split("[\\(\\[]")(0).trim
        val normalizedAlbum = normalizeAlbumName(cleanAlbum)
        // Add "(cracktro)" suffix if path contains "/Cracktro/"
        album = if (path.startsWith("Music - Games - Cracktro/")) {
          s"$normalizedAlbum (cracktro)"
        } else {
          normalizedAlbum
        }
      case None =>
        albumNoSeparatorPattern.findFirstMatchIn(filename).foreach { m =>
          val normalizedAlbum = normalizeAlbumName(m.group(1).trim)
          album = if (path.startsWith("Music - Games - Cracktro/")) {
            s"$normalizedAlbum (cracktro)"
          } else {
            normalizedAlbum
          }
        }
    }
  }
  
  // If no sequencer, composers become authors
  if (authors.isEmpty && composers.nonEmpty) {
    authors ++= composers
  }

  if (publishers.size == 1 && publishers.head == "JMP") {
    publishers.clear
    authors += "JMP"
  }
  
  if (authors.size == 1 && authors.head == "Fire by HMW") {
    authors.clear
    authors += "HMW"
  }

  // check if author name ends with year, like Merci(1991), indicating remix
  if (authors.size == 1 && authors.head.matches(raw".*\(\d{4}\)$$")) {
    val authorWithYear = authors.head
    val yearInParensPattern = raw"\((\d{4})\)$$".r
    yearInParensPattern.findFirstMatchIn(authorWithYear).foreach { m =>
      year = m.group(1).toInt
    }
    authors.clear()
    authors += authorWithYear.replaceFirst(raw"\(\d{4}\)$$", "").trim
    publishers.clear()
    album = ""
  }
  
  Some(TosecMeta(authors.filterNot(_.trim.isEmpty).sorted.distinct, composers.filterNot(_.trim.isEmpty).sorted.distinct, publishers.filterNot(_.trim.isEmpty).sorted.distinct, album.trim, year))
}
