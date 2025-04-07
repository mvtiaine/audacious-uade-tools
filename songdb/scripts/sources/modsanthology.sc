// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

// This got a bit out of hand...

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.Using
import scala.util.boundary, boundary.break

val modsanthology_path = System.getProperty("user.home") + "/modsanthology/"

case class ModsAnthologyMeta (
  md5: String,
  path: String,
  filesize: Int,
  songlength: Option[Int], // seconds
  authors: Buffer[String],
  publishers: Buffer[String],
  album: String,
  year: Option[Int]
)

val authorPatterns = Seq(
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)/\\w+ [Ff]rom".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)/\\w+ [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)/\\w+ \\([Hh]is ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)\\s+[Ff]rom".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) \\([Oo]nly\\)".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) [Oo]nly".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) [Ii]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) [Aa]t ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) [Oo]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) \\(".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)'$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)'\\(".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+, \\w+ and \\w+ \\w+) [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+),".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) [Aa]lone".r,
  "^(?:\\d{2} ch - )?[Tt]une [Bb]y (\\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ & \\w+ \\w+) [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ & \\w+-\\w+) [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ & \\w+ \\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ & \\w+ [A-Za-z0-9\\.]+) / \\w+ ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ & \\w+-\\w+)".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ & \\w+)".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) - ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) / \\w+ \\w+ [Oo]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) / \\w+ [Oo]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)/\\w+ [Oo]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+) - ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)! - ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)! [Ii]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)! [Oo]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)! [Ff]rom ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)! [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+! & \\w+ \\w+) - ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)!$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)! \\(".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+) [Ff]rom ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ [Tt]he \\w+) / ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+! & \\w+ \\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+! & \\w+ \\w+ & \\w+)".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+) \\(".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+! & \\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+) [Ff]or ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) \\d{2}$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+) \\d{4}$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+) [Ii]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+ \\w+) [Oo]n ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+)..[Ff]rom ".r,
  "^(?:\\d{2} ch - )?[Bb]y (\\w+/\\w+ & \\w+/\\w+) [Ff]or ".r,
  ".*, (w+) [Oo]nly$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ [Ff]rom".r,
  ".* [Mm]odule - [Bb]y (\\w+)/\\w+ [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+'[a-zA-Z] \\w+) [Ii]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ë)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+'[a-zA-Z] \\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+'[a-zA-Z] \\w+) [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ [a-zA-Z]'\\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) - ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ë) - ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ë) / ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ë) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) '\\d{2}".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) [Xx]mas '\\d{2}".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) '\\d{2}".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+, ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+) \\(".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+), ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+) - ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+) [Ii]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+) [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+.\\w+) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+. \\w+) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ & \\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ \\w+ [Oo]n".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) / \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) / \\w+ \\(!\\)".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) [Oo]f \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) [Oo]f \\w+ \\(".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) [Oo]f \\w+ - ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) \\d{2}/\\d{2}$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+)/\\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+)/\\w+ \\(".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ \\w+ [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ & \\w+ \\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ & \\w+ \\w+) '".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ & \\w+.\\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ & \\w+.\\w+) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+/\\w+ & \\w+/\\w+) [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) \\d{2}-\\w{3}-\\d{2}".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ & \\w+) '\\d{2}".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) ! \\(".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ & \\w+ [Oo]f ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+-\\w+/\\w+ & \\w+/\\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+!$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) / [a-zA-Z.]+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+.)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ & \\w+ \\([Oo]f ".r,
  "^[Ff]rom .* \".*\" - [Bb]y (\\w+ \\w+ \\w+)$".r,
  "^[Ff]rom .* \".*\" - [Bb]y (\\w+ \\w+)$".r,
  "^[Ff]rom .* \".*\" - [Bb]y (\\w+)$".r,
  "^[Ff]rom .* \".*\" - [Bb]y (\\w+!) \\(".r,
  "^[Aa]ka .* - [Bb]y (\\w+)$".r,
  ".* [Tt]une [Bb]y (\\w+)/\\w+".r,
  "^Hhmmm... [Bb]y (\\w+) / \\w+ \\w+ \\(".r,
  ".* [Pp]art - [Bb]y (\\w+ \\w+)$".r,
  ".* [Pp]art - [Bb]y (\\w+ \\w+) \\(".r,
  ".* [Pp]art - [Bb]y (\\w+) / \\w+ \\w+$".r,
  ".* [Pp]art - [Bb]y (\\w+) / \\w+ & \\w+$".r,
  ".* [Pp]art - [Bb]y (\\w+) \\(".r,
  ".* [Pp]art - [Bb]y (\\w+ & \\w+) / \\w+$".r,
  "^[Aa]dapted [Bb]y (\\w+)$".r,
  "^[Ff]rom .* \".*\", [Aa]ll [Mm]usic [Bb]y (\\w+ & \\w+) \\(".r,
  "^[Ff]rom [Tt]he \".*\" [Mm]ega[Dd]emo, [Bb]y (\\w+ \\w+)!".r,
  "^[Oo]riginal [Bb]y .* - [Aa]rranged [Bb]y (\\w+)/\\w+$".r,
  "^[Bb]y (\\w+.\\w+) / \\w+ [Oo]n ".r,
  "^[Bb]y (\\w+.\\w+) / \\w+ '".r,
  "^[Bb]y ([A-Za-z\\.]+) [Oo]f \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) !".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+) \\?".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) \\?".r,
  "^(?:\\(.*\\) )?[Ff]rom \\w+ \".*\" - [Bb]y (\\w+)$".r,
  "^[Bb]y ([A-Za-z\\.]+)/\\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+ \\w+)/\\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+) [Oo]f \\w+ \\w+$".r,
  "^[Bb]y (\\w+-\\w+) [Ff]or ".r,
  "^[Bb]y (\\w+) / \\w+ \\(".r,
  "^[Bb]y (\\w+)/\\w+ [0-9]+$".r,
  "^[Bb]y (\\w+) / \\w+ [Ff]or ".r,
  "^[Bb]y ([A-Za-z\\.]+ \\w+) [Ff]or ".r,
  "^[Bb]y (\\w+) [Oo]f \\w+ '".r,
  "^[Bb]y (\\w+) [Oo]f \\w+ [Ii]n ".r,
  "^[BB]y (\\w+ \\w+ \\w+) [Oo]f ".r,
  "^[Bb]y (\\w+ \\w+ \\w+) [Ii]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ \\(".r,
  "^[Bb]y (\\w+)/\\w+ & \\w+ ".r,
  "^[Bb]y (\\w+) [Oo]f \\w+-\\w+$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ and \\w+) \\(".r,
  "^[Bb]y (\\w+ and \\w+) / \\w+ [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+/\\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+/\\w+ \\(".r,
  "^[Bb]y (\\w+\\$) [Ii]n ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+ [Ii]n ".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ [0-9]+$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ \\(".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ '".r,
  "^[Bb]y \\w+ '(\\w+)' \\w+ - ".r,
  "^[Bb]y \\w+ '(\\w+)' \\w+ '".r,
  "^[Bb]y \\w+ '(\\w+)' \\w+ [Oo]n ".r,
  "^[Bb]y ([A-Za-z\\.]+) '".r,
  "^[Bb]y (\\w+)/\\w+-\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+ \\w+ \\w+ [Ii]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+ \\w+) / \\w+$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ [0-9]+ [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+-\\w+)/\\w+ [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+-\\w+)/\\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ [Ff]or ".r,
  "^[Bb]y (\\w+-\\w+) / \\w+$".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ '".r,
  "^[Bb]y (\\w+ [A-Za-z]\\. \\w+)$".r,
  "^[Bb]y (\\w+ [0-9]+) / \\w+ [Ff]or ".r,
  "^[Bb]y ([A-Z]\\. \\w+ \\w+) '".r,
  "^[Bb]y (\\w+)/\\w+ [0-9]+ [Oo]n ".r,
  "^[Bb]y (\\w+)/\\w+ [0-9]+ [Ff]rom ".r,
  "^[Bb]y (\\w+ \\w+)/\\w+ [Oo]n ".r,
  "^[Bb]y (\\w+)/\\w+ [0-9]+ [Tt]he \\w+ '".r,
  "^[Bb]y (\\w+ \\w+)/\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+'\\w+'\\w+) / \\w+ [Ff]or ".r,
  "^[Bb]y (DJ \\w+ \\w+) '".r,
  "^[Bb]y (DJ \\w+ \\w+)$".r,
  "^[Bb]y (\\w+)/\\w+-[0-9]+ [Ii]n ".r,
  "^[Bb]y (\\w+)/\\w+-[0-9]+ [Ff]rom ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+ [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+) / \\w+ [Ff]rom ".r,
  "[Bb]y ([A-Z] & [A-Z]) / \\w+$".r,
  "^[Bb]y (\\w+)/\\w+ & \\w+$".r,
  "^[Bb]y (\\w+.\\w+ & \\w+)$".r, // By Loïc & Romu
  "^[Bb]y (\\w+ \\w+) \\d{4}$".r,
  "^[Bb]y (\\w+ \\w+ [A-Z]\\.) '".r,
  "^[Bb]y (\\w+ \\w+ & [A-Za-z\\.]+) -".r,
  "^[Bb]y (\\w+) [Oo]f \\w+/".r,
  "^[Bb]y (\\w+) [Oo]f \\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ [Tt]he \\w+) \\(".r,
  "^[Bb]y (\\w+ \\w+ \\w+) [Ff]or ".r,
  "^[Bb]y ([A-Za-z\\.]+) [Ff]or ".r,
  "^[Bb]y (\\w+)/\\w+ [0-9]+ & \\w+$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+)/\\w+!\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+)/\\w+!\\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ [Oo]n ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ [Ii]n ".r,
  "^[Bb]y (\\w+)/\\w+\\^".r,
  "^[Bb]y (\\w+) / \\w+ \\w+,".r,
  "^[Bb]y (\\w+ \\w+) [Oo]f \\w+ [Ff]or ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ / \\w+$".r,
  "^[Bb]y (\\w+ \\w+) [Aa]dapted [Ff]rom ".r,
  "^[Bb]y (\\w+)/\\w+\\.\\w+ [Oo]n ".r,
  "^[Bb]y (\\w+) / \\w+,".r,
  "^[Bb]y (\\w+ \\w+)/\\w+,".r,
  "^[Bb]y (\\w+)/\\w+-\\w+ [Oo]n ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+ \\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+) [Oo]f \\w+-\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ [A-Za-z]'\\w+) / \\w+ '".r,
  "^[Bb]y (\\w+) / \\w+ [0-9]+ \\(".r,
  "^[Bb]y (MC \\w+ \\w+) '".r,
  "^[Bb]y (\\w+) [Oo]f \\w+ \\w+ '".r,
  "^[Bb]y ([A-Za-z\\.]+)$".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ [0-9]+ [Tt]he ".r,
  "^[Bb]y (\\w+ \\w+ & [A-Za-z\\.]+)$".r,
  "^[By]y (\\w+ [A-Z]\\.) / [A-Za-z\\.]+ [Ff]rom ".r,
  "^[Bb]y (\\w+)/\\w+ \\w+. [Ff]or ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ \\w+$".r,
  "^[Bb]y (\\w+\\. \\w+)$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ [Ii]n ".r,
  "^[Bb]y (\\w+\\.\\w+)/\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ \\(".r,
  "^[Bb]y ([A-Za-z\\.]+)/\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ \\w+\\. [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ \\w+ \\w+ [Ff]rom ".r,
  "^[Bb]y (\\w+)! - [Mm]egademo".r,
  "^[Bb]y (\\w+) / \\w+ - ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ \\w+ :".r,
  "^[Bb]y (\\w+)/\\w+! [Ff]or ".r,
  "^[Bb]y (\\w+)/\\w+! [Ff]rom ".r,
  "^[Bb]y ([A-Za-z\\.]+) / \\w+$".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ \\w+ '".r,
  "^[Bb]y (\\w+&\\w+)/\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+)/\\w+/\\w+ \\w+ - ".r,
  "^[Bb]y ([A-Za-z0-9\\.]+) [Oo]f ".r,
  "^[Bb]y (\\w+ [A-Za-z\\.]+ \\w+)$".r,
  "^[Bb]y (\\w+)/\\w+\\+\\w+ '".r,
  "^[Bb]y ([A-Za-z\\.]+) / \\w+ [Ff]or ".r,
  "^[Bb]y (\\w+-\\w+)/\\w+,".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+-\\w+)/\\w+/\\w+$".r,
  "^[Bb]y (\\w+)! \\(".r,
  "^[Bb]y (\\w+)/[A-Za-z\\.]+ !!".r,
  "^[Bb]y ([Oo]'\\w+)/\\w+$".r,
  "^[Bb]y (\\w+ \\w+) [Aa]t ".r,
  "^[Bb]y (\\w+-\\w+) [Oo]f ".r,
  "^[Bb]y (\\w+ \\w+ & \\w+ \\w+) / \\w+ [Ii]n ".r,
  "^[Bb]y (\\w+\\+\\w+) '".r,
  "^[Bb]y (\\w+) / \\w+!$".r,
  "^[Bb]y ([A-Za-z\\.]+)/\\w+-\\w+ [Ff]or ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+ \"(.*)\" \\w+ \\(.*\\) [Ff]rom ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+, ".r,
  "^[Bb]y (\\w+ \\w+\\.) \\(".r,
  "^[Bb]y (\\w+)!\\?$".r,
  "^[Bb]y \\w+ \\w+' [Aa]lias (\\w+)$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+ \\w+!$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+\\.$".r,
  "^[Bb]y (\\w+.\\w+)/\\w+ [Ff]or ".r,
  "^[Bb]y (A's) [Oo]f \\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+ \\w+$".r,
  "^[Bb]y (\\w+ \\w+) [Oo]f \\w+$".r,
  "^[Bb]y (\\w+)/\\w+-\\w+$".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ !$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+\\. \\w+) / \\w+-\\w+$".r,
  "^[Bb]y (\\w+)/\\w+ \\w+\\. [Ff]rom ".r,
  "^[Bb]y (\\w+ \\w+\\.) [Ff]or ".r,
  "^[Bb]y (\\w+-\\w+) / \\w+ \\(".r,
  "^[Bb]y (\\w+.\\w+) [Ff]or ".r,
  "^[Bb]y (\\w+) / \\w+ ,".r,
  "^[Bb]y (\\w+ \\w+ & \\w+) [Oo]f ".r,
  "^[Bb]y (\\w+) [Ww]ith ".r,
  "^[Bb]y ([A-Za-z\\.]+ & \\w+)/\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+)/\\w+ [A-Za-z\\.]+ [Ff]or ".r,
  "^[Bb]y (\\w+) / \\w+ \\w+ / \\w+ \\w+ [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+)/\\w+ \\w+/[A-Za-z\\.]+$".r,
  "^[Bb]y (\\w+) / \\w+ / \\w+-\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ \\w+) / \\w+.\\w+ [Ff]or ".r,
  "^[Bb]y (\\w+ \\w+)/\\w+ -".r,
  "^[Bb]y (\\w+)/\\w+ [A-Za-z0-9\\.]+$".r,
  "^[Bb]y (\\w+-\\w+ \\w+)$".r,
  "^[Bb]y (\\w+ \\w+ \\w+)$".r,
  "^© (\\w+) -".r,
  "^[Aa]dapted [Ff]rom \\w+ [Bb]y (\\w+)/\\w+$".r,
  "^(?:.* ch - )?[Bb]y (\\w+) [Oo]f \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y ([A-Za-z0-9\\.]+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y ([A-Za-z0-9\\.]+ \\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+)/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ \\w+/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+) / \\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ & \\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ \\w+' \\w+ \\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y ([A-Za-z0-9\\.]+)/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+) / \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) / \\w+ & \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ [A-Za-z0-9\\.]+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+-\\w+)/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ & \\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)!/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+)/\\w+.[Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) / \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) / \\w+ [0-9]+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+-\\w+) / \\w+ [0-9]+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ \\w+) / \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+-\\w+) [Oo]n ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ [Ii]n ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ - ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+)/\\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+!\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+-\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) [Oo]f \\w+/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ \\w+)/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+-\\w+)/\\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+-\\w+)/\\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+\\..\\w+) [Oo]f \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+/\\w+ and \\w+ \\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+) [Oo]f \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+.[Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+.\\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) [Oo]f \\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) [Aa]ka \\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ \\w+ \\w+) [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ \\w+ \\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y ([A-Za-z0-9\\.]+)/[A-Za-z0-9\\.]+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+.\\w+)/\\w+ [Ff]or ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ \\(.ch.\\) ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/[a-zA-Z0-9!]+ [Aa]nd [a-zA-Z0-9!]+ [Oo]n ".r,
  "^(?:.* ch - )?[Bb]y (\\w+) / \\w+$".r,
  "^(\\w+) [Ff]or ".r,
  "\".*\" (\\w+)/\\w+$".r,
  ", [Bb]y (\\w+) [Aa]t ".r,
  "^[Bb]y ([A-Za-z0-9\\.]+)/\\w+ [Oo]n ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+) / \\w+$".r,
  "^(?:.* ch - )?[Bb]y (\\w+) '\\d{2}$".r,
  "^[Aa]dapted [Bb]y (\\w+) [Ii]n '\\d{2}$".r,
  "^[Aa]dapted [Bb]y (\\w+ \\w+) '\\d{2}$".r,
  "^[Aa]dapted [Bb]y (\\w+ \\w+) '\\d{2} [Ff]rom ".r,
  "^[Aa]dapted [Bb]y (\\w+) [Ff]rom ".r,
  "^[Rr]emix [Bb]y (\\w+)$".r,
  "^(?:\\(.*\\) )?[Bb]y (\\w+ \\w+)/\\w+/\\w+$".r,
  "^[Dd]elire [Bb]y (\\w+ & \\w+) !".r,
  "^[Rr]emixed [Bb]y (\\w+ & \\w+)$".r,
  "^[Dd]elire [Dd]e (\\w+ \\w+ & \\w+ \\w+) !".r,
  "^[Dd]elire [Dd]e (\\w+ \\w+) !".r,
  "^[Rr]emix [Bb]y (\\w+) [Aa]t ".r,
  "^\".*\" [Bb]y (\\w+)$".r,
  "^\".*\" [Bb]y (\\w+.\\w+)$".r,
  "^[Rr]emixed [Bb]y (\\w+) [Oo]n ".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+$".r,
  "^(?:\\(.*\\) - )?[Bb]y (\\w+)$".r,
  "^(?:.* ch - )?[Bb]y (\\w+)/\\w+ \\(".r,
  "^[Aa]dapted [Bb]y (\\w+ \\w+)$".r,
  "^[Aa]dapted [Bb]y (\\w+ \\w+) [Ff]rom ".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+) '".r,
  "^(?:.* ch - )?[Bb]y (\\w+ \\w+ \\w+)$".r,
  "^[Bb]y (\\w+ \\w+ \\w+) / \\w+ [Oo]n ".r,
  "^\\w+! [Bb]y (\\w+)/\\w+! ".r,
  "^[Bb]y (\\w+ \\w+ \\w+) \\(.*\\)$".r,
  "^[Bb]y (\\w+\\. [a-zA-Z\\.]+ \\w+) [Ff]rom ".r,
  "^[Bb]y (\\w+ \\w+) [Oo]f \\w+ & \\w+ [Ff]rom ".r,
  "^[Rr]emix [Bb]y (\\w+ \\w+) '\\d{2} [Ff]rom ".r,
  "^[Bb]y (\\w+)/\\w+! '".r,
  "- [Bb]y (\\w+)$".r,
  "- [Bb]y ([a-zA-Z\\.]+)$".r,
  "[Mm]usics [Bb]y (\\w+ \\w+)!! ".r,
  "[Mm]usics [Bb]y (\\w+ \\w+)\\.\\.\\.".r,
  "[Mm]usic [Bb]y (\\w+ \\w+)$".r,
  "[Mm]usic [Bb]y (\\w+) [Ff]rom ".r, 
  "- [Bb]y (\\w+ \\w+)$".r,
  "[Mm]uzak [Bb]y (\\w+ \\w+ \\w+ \\w+)!".r,
  "[Mm]aster[Pp]iece [Bb]y (\\w+ \\w+ \\w+ \\w+)!".r,
  "\\) (\\w+ \\w+ \\w+ \\w+) [Rr]ulez".r,
  "! \\((\\w+ \\w+ \\w+ \\w+) [Aa]gain\\)".r,
  "^[Bb]y ([a-zA-Z\\.]+) - ".r,
  "- [Bb]y (\\w+) [Ff]or ".r,
  "^[Bb]y (\\w+ \\w+ \\w+) / \\w+ \\(".r,
  "- [Bb]y (\\w+ \\w+) [Oo]n ".r,
  "[Ff]rom \\w+ \".*\" [Ii]n '\\d{2} \\((\\w+ & \\w+)\\)$".r,
)

val coopPatterns = Seq(
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+)$".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+ \\w+)$".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) for".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) in ".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+ \\w+) in '".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) on ".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) \\(".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) -".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+),".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+)/\\w+$".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+ \\w+)!$".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) / \\w+".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+ & \\w+) / \\w+".r, 
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+)/\\w+".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+)!$".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+ \\w+)$".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) [Ff]rom".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+) [Ff]or".r,
  "^(?:\\d{2} ch - )?[Ww]ith (\\w+-\\w+)$".r,
  "^\\(.*\\) [W]ith (\\w+)$".r,
  "[Ii]n '\\d{2} - [Ww]ith (\\w+)!$".r,
  "[Ii]n '\\d{2} - [Ww]ith (\\w+)$".r,
  "[Ii]n '\\d{2} - [Ww]ith (\\w+ & \\w+)$".r,
  "[Ii]n \\w{3} '\\d{2} - [Ww]ith (\\w+)$".r,
  "[Ii]n '\\d{2} [Ww]ith (\\w+)!$".r,
  "[Ii]n '\\d{2} [Ww]ith (\\w+)$".r,
  "[Ii]n '\\d{2} [Ww]ith (\\w+) \\(".r,
  "[Ii]n \\w{3} '\\d{2} [Ww]ith (\\w+)$".r,
  "[Ii]n \\w{3} '\\d{2} [Aa]t .*, [Ww]ith (\\w+ & \\w+)".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2}.*[Ww]ith (\\w+)$".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2}.*[Ww]ith (\\w+ \\w+)$".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2}.*[Ww]ith (\\w+ \\w+) -".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2}.*[Ww]ith (\\w+) -".r,
  "[Ff]rom .*, [Ww]ith (\\w+)$".r,
  "[Ff]rom .*, [Ww]ith (\\w+ & \\w+)$".r,
  "[Ff]rom .*, [Ww]ith (\\w+ \\w+)$".r,
  "[Ff]rom .*, [Ww]ith (\\w+)/\\w+$".r,
  "- \\([Ww]ith (.*)\\)$".r,
  "^(?:\\d{2} ch - )?[Bb]y .*/.* & (.*)/.*".r,
  "^(?:\\d{2} ch - )?[Bb]y \\w+ [Aa]nd (\\w+)".r,
  "^(?:\\d{2} ch - )?[Bb]y \\w+ & (\\w+) [Ff]or".r,
  "^(?:\\d{2} ch - )?[Bb]y \\w+ & (\\w+),".r,
  "^(?:\\d{2} ch - )?[Bb]y \\w+ & (\\w+)$".r,
  "^(?:\\d{2} ch - )?[Bb]y \\w+ & (\\w+) Design$".r,
  "^(?:\\d{2} ch - )?[Bb]y .* [Ff]rom \".*\" [Ww]ith (\\w+ & \\w+)".r,
  "[Ww]ith (\\w+-\\w+)'".r,
  "[Ww]ith (\\w+) [Ff]rom ".r,
  "[Ww]ith ([a-zA-Z|.]+) / ".r,
  "[Ii]n \\w{3} '\\d{2} [Ww]ith (\\w+)/".r,
  "[Ww]ith ([a-zA-Z|.]+)$".r,
  "[Ww]ith ([a-zA-Z|.]+) in ".r,
  "[Ww]ith (\\w+)/\\w+!".r,
  "[Bb]y \\w+ [Ww]ith (\\w+ & \\w+)$".r,
  "^\\(.*\\) [Bb]y (\\w+ \\w+)/\\w+ [Ff]rom ".r,
)

val yearPatterns = Seq(
  "[Oo]n \\d{1,2}-\\w{3}-\\d{2,4}, [Tt]hen \\d{1,2}-\\w{3}-(\\d{2,4})".r,
  "[Ii]n '\\d{2}, [Rr]eleased [Ii]n '(\\d{2})".r,
  "[Ii]n '\\d{2}, [Rr]emixed [Ii]n '(\\d{2})".r,
  "[Ii]n (\\d{4})".r, // In 1996
  "[Ii]n \\w{3}/\\w{3}-(\\d{2})".r, // In Feb/Mar-93
  "[Ii]n '\\d{2} & (\\d{2})".r, // In '92 & 95
  "[Ii]n '\\d{2}-(\\d{2})".r,
  "[Ii]n '\\d{2}/(\\d{2})".r,
  "[Ii]n '(\\d{2})".r,
  " '\\d{2}-(\\d{2})$".r, // '96-97
  " '\\d{2}-(\\d{2})[, ]".r, // '96-97, ...
  " '\\d{2}/(\\d{2})$".r, // '96/97
  " '\\d{2}/(\\d{2})[, ]".r, // '96/97, ...
  "\\d{1,2}-\\w{3}-(\\d{2,4})".r, // 01-Jan-1996
  "\\d{1,2}/\\d{1,2}-\\w{3}-(\\d{2,4})".r, // 12/13-Jan-94
  "\\((\\d{4})\\)".r, // (1996)
  " - \\d{2}/(\\d{2})".r, // - 01/96
  "[Ff]rom .*'(\\d{2}).* [Dd]emo".r, // From JollyWorker Asm'95 Demo
  "[Ff]rom .* TP(\\d{2}).* [Dd]emo".r, // From Stellar "Galerie" TP95 demo (Amiga)
  "[Ff]rom .* TP(\\d{2}).* [Ii]ntro".r, // From Orange "The sea robot of love" TP95 charted 3 intro
  "[Ff]rom .* \".*\" \\d{2}/(\\d{2})".r,
  "[Ff]rom .* \".*\" (\\d{4})".r,
  "[Ff]rom .* \".*\" (\\d{2})".r,
  "[Ff]rom [Tt]he (\\d{4})".r,
  "[Ff]or .* \\(TP(\\d{2})\\)".r, // (P60A) For Exceed "Mosaic" SlideShow (TP94) 
  " '(\\d{2})$".r, // '96
  " '(\\d{2})[, ]".r, // '96, ...
  " '(\\d{2})!$".r, // Won The Assembly '93!
  "[0-1][0-9]/([8-9][0-9])".r, // 07/91
  "[Ss]ummer 19(\\d{2})".r, // Summer 1991
  "[Ss]ummer '(\\d{2})".r,
  "[Ss]ummer'(\\d{2})".r,
  "[Ss]pring 19(\\d{2})".r,
  "[Ss]pring '(\\d{2})".r,
  "[Ss]pring'(\\d{2})".r,
  "[Ww]inter 19(\\d{2})".r,
  "[Ww]inter '(\\d{2})".r,
  "[Ww]inter'(\\d{2})".r,
  "[Aa]sm'(\\d{2})".r,
  "^([8-9][0-9]) -".r,
  "^([8-9][0-9])$".r,
  "[W]on \\w+ \\w+ '(\\d{2})".r,
  "[Ff]or [Tt][Gg](\\d{2})".r,
  "©(\\d{4})".r,
  "© \\w+ -(\\d{2})$".r,
  "^[Bb]y \\w+/\\w+ (\\d{2})$".r,
  "^[Bb]y \\w+/\\w+ (\\d{4})$".r,
  "\\(\\w{3} '(\\d{2})\\)$".r,
  "^(?:\\d{2} ch - )?[Bb]y \\w+ (\\d{2})$".r,
  "[Bb]y \\w+/\\w+ [Oo]n \\d{2}.\\d{2}.(\\d{2})".r,
  "[Ff]rom \\w+ \".*\" \\(\\w+ '(\\d{2})\\)".r,
  ".* '(\\d{2}).$".r,
  "[Ii]n \\w{3}' (\\d{2})$".r,
  "[Ii]n \\w{3} (\\d{2})$".r,
  ".* \\w+/\\w+ (\\d{4})$".r,
  "^[Bb]y \\w+ (\\d{4})$".r,
  "^[Bb]y \\w+/\\w+ \\w{2} (\\d{2})$".r,
  "[Ii]n \\w+ (\\d{2})$".r,
  "[Ii]n \\w+\\.(\\d{2})$".r,
  "[Oo]n \\d{2}.\\d{2}.(\\d{2})$".r,
  "[Ii]n \\w+ '(\\d{2})!".r,
)

val albumPatterns = Seq(
  "[Ff]rom the (?:1st)? .* AGA [Dd]emo \"(\\w+ \\w+)\"".r,
  "[Ff]rom the (?:1st)? .* AGA [Dd]emo \"(\\w+)\"".r,
  "[Ff]rom the (?:2nd)? .* AGA [Dd]emo \"(\\w+ \\w+)\"".r,
  "[Ff]rom the (?:2nd)? .* AGA [Dd]emo \"(\\w+)\"".r,
  "[Ff]rom ([Rr][Aa][Ww] [#]?[0-9]+)".r,
  "[Ff]rom (Zine [#]?[0-9]+)".r,
  "[Ff]rom (Stolen[ |-]Data[ |-][#]?[0-9]+)".r,
  "[Ff]rom ([Tt]he \\w+ #[0-9]+$)".r,
  "[Ff]rom ([Tt]he \\w+ #[0-9]+) -".r,
  "[Ff]rom ([Tt]he \\w+ #[0-9]+) \\(".r,
  "[Ff]rom .* (Music-Disk #[0-9]+)".r,
  "[Ff]rom (\\w+ #[0-9]+) in".r,
  "[Ff]rom (\\w+ #[0-9]+)$".r,
  "[Ff]rom .* & \\w+ \"(.*)\" [Dd]emo".r,
  "[Ff]rom (\\w+ #[0-9]+) \\(".r,
  "[Ff]rom \"(\\w+ #[0-9]+)\" in".r,
  "[Ff]rom \"(\\w+ #[0-9]+)\"$".r,
  "[Ff]rom \"(.*)\" [Bb]y ".r, // From "3 from 1" by Chryseis"
  "[Mm]ade [Ff]or \\w+ \\w+ \"(.*)\"".r,
  "[Mm]ade [Ff]or \\w+ \"(.*)\"".r,
  "[Ff]or ([Rr][Aa][Ww] [#]?[0-9]+)".r,
  "[Ff]or (Zine [#]?[0-9]+)".r,
  "[Ff]or (Stolen[ |-]Data[ |-][#]?[0-9]+)".r,
  "[Ff]or (The Euro[Cc]harts [#]?[0-9]+)".r,
  "[Ff]or .*'s [Dd]emo \"(.*?)\"".r, //  In '96 for Tran's demo "Luminati" - Great!
  "[Ff]or [Aa] .* \"(.*?)\" [Ii]ssue".r, // For a DRD "Live" Issue...
  "[Ff]or ([Tt]he \\w+ #[0-9]+$)".r,
  "[Ff]or ([Tt]he \\w+ #[0-9]+) -".r,
  "[Ff]or ([Tt]he \\w+ #[0-9]+) \\(".r,
  "[Ff]or (\\w+ #[0-9]+) in".r,
  "[Ff]or (\\w+ #[0-9]+)$".r,
  "[Ff]or \"(\\w+ #[0-9]+)\" in".r,
  "[Ff]or \"(\\w+ #[0-9]+)\"$".r,
  "[Ff]or .* \"(.*)\" in".r,
  "[Ff]or (.* #[0-9]+) /".r,
  "[Ff]or .* ([Mm]ega[Dd]emo [0-9IiVv]+)$".r,
  "[Ff]or .* ([Mm]ega[Dd]emo)$".r,
  "[Ff]or .* ([Mm]ega[Dd]emo [0-9IiVv]+) \\(".r,
  "[Ff]or .* ([Mm]ega[Dd]emo) \\(".r,
  "[Ff]or .* ([Mm]ega[Dd]emo [0-9IiVv]+) -".r,
  "[Ff]or .* ([Mm]ega[Dd]emo) -".r,
  "[Ff]or .* ([Mm]ega[Dd]emo [0-9IiVv]+),".r,
  "[Ff]or \"\\w+ (Megademo)\" in".r,
  "[Ff]or \"\\w+ (Megademo [0-9IiVv]+)\" in".r,
  "[Ff]or .* \"(.*)\" [Dd]entro".r,
  "[Ff]or .* \"(.*)\" [Ss]lide[Ss]how".r, //  (P60A) For Exceed "Mosaic" SlideShow (TP94)
  "[Ff]or .* \"(.*)\".*[Dd]emo".r,
  "[Ff]or \"(.*)\" [Bb]y ".r,
  "(.*?) [Ee]nd[Tt]une".r, // Flower Power EndTune
  ".* - [Uu]sed \"(.*?)\"".r, // The Original module! - Used "In The Kitchen" :) 
  "[Uu]sed [Ii]n [Bb]oth .* \"(.*?)\"".r,
  "[Uu]sed [Ii]n \\w+ \"(.*?)\"".r, 
  "[Uu]sed [Ii]n \\w+ \\w+ (\".*\" #[0-9]+)".r, 
  "[Uu]sed [Ii]n \\w+ \\w+ \"(.*?)\"".r, 
  "[Uu]sed [Ii]n (\\w+ #[0-9]+)$".r,
  "[Uu]sed [Ii]n .* (\".*\" [Ii]ssue #[0-9]+) \\(".r,
  "[Ff]ound [Ii]n \\w+ \"(.*?)\"".r, // Found in Silicon "Birthday" - Hidden Muzak ;-)
  "[Ff]eatured in .* \"(.*?)\"".r,
  "[Ff]rom .* \"(.*)\" [Dd]emo \\(.*'.*\\)".r,
  "[Ff]rom .* \".*\" [Dd]emo \\((.*)\\)$".r,
  "[Ff]rom (Bruno's Music Box [0-9]+)".r,
  "[Ff]rom (.*)/.* [Cc]hip[Dd]isk".r,
  "[Ff]rom (.*?)/.* [Dd]emo".r, // From Charles 2000/Banal Projects demo 
  "[Ff]rom .* ([Mm]ega[Dd]emo [0-9IiVv]+)$".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo)$".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo [0-9IiVv]+) \\(".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo) \\(".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo [0-9IiVv]+) -".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo) -".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo [0-9IiVv]+),".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo),".r,
  "[Ff]rom .* ([Mm]ega[Dd]emo)!".r,
  "[Ff]rom \"\\w+ ([Mm]ega[Dd]emo)\" in".r,
  "[Ff]rom \"\\w+ ([Mm]ega[Dd]emo [0-9IiVv]+)\" in".r,
  "[Ff]rom [Th]he \\d{4} \\w+ ([Mm]ega[Dd]emo)".r,
  "[Ff]rom [Th]he \\d{4} \\w+ ([Mm]ega[Dd]emo [0-9IiVv]+)".r,
  "[Ff]rom .*\"(.*)\" [Ii]ntro".r,
  "[Ff]rom .*\"(.*)\" [Dd]emo".r,
  "[Ff]rom \\w+ (Tune-Disk [0-9])".r,
  "[Ff]rom \"(.*?)\" [Bb]y ".r, // From "3 from 1" by Chryseis
  "[Ff]rom \"(.*?)\" [Ii]n ".r, 
  "[Ff]rom \"(.*?)\" [Oo]n ".r, 
  "[Ff]rom \"(.*?)\" [Aa]t ".r, 
  "[Ff]rom \"(.*?)\" .*[Dd]emo$".r,
  "[Ff]rom \"(.*?)\" .*[Tt]rackmo$".r,
  "\\([Ff]rom \"(.*?)\"\\)".r,
  "(?:\\(.*\\) )?[Ff]rom \"(.*?)\" [Mm]usic[-]?[Dd]isk$".r,
  "(?:\\(.*\\) )?[Ff]rom \"(.*?)\" [Mm]usic[-]?[Dd]isk - ".r,
  "(?:\\(.*\\) )?[Ff]rom \"(.*?)\" [Mm]usic[-]?[Dd]isk, ".r,
  "(?:\\(.*\\) )?[Ff]rom \"(.*?)\" [Mm]usic[-]?[Dd]isk \\(".r,
  "[Bb]y .*, [Ff]rom \"(.*?)\" [Mm]usic[-]?[Dd]isk$".r,
  "[Bb]y .* - [Ff]rom \"(.*?)\" [Mm]usic[-]?[Dd]isk$".r,
  "[Bb]y \\w+ \\w+ [Ff]rom \"(.*?)/.*\"$".r,
  "[Bb]y [A-Za-z0-9\\.]+ [Ff]rom \"(\\w+)/\\w+\"$".r,
  "[Bb]y \\w+ \\w+ [Ff]rom \"(.*?)\"$".r,
  "[Bb]y \\w+ \\w+ [Ff]rom \"(.*?)\" [Ww]ith ".r,
  "[Bb]y \\w+ [Ff]rom \"(.*?)/.*\" ".r,
  "[Ww]ith \\w+ [Ff]rom \"(.*?)\" ".r,
  "[Ff]rom \"(.*)\" [Ii]ntro$".r,
  "[Ff]rom [Tt]he [Ii]ntro \"(.*)\"".r,
  "[Ff]rom ([Tt]he \\w+ #[0-9]+) -".r, // From The Eurocharts #9 - 03/91
  "[FF]rom \"(.*)\" [Aa]t ".r,
  "[Pp]art [aA-zZ] [Oo]f [Tt]he \"(.*)\" [Ss]erie".r,
  "[Pp]art [Oo]f [Tt]he '(.*)' [Ss]erie".r,
  "[Tt]he \"(.*)\" [Ss]erie".r,
  "[Ff]rom (.*?) [Bb]y \\w+ \\w+$".r,
  "[Uu]sed [Ii]n \\w+ (.*) \\(mini\\) [Mm]usic-[Dd]isk".r,
  "[Uu]sed [Ii]n (\\w+ \\w+ #[0-9]+) [Ii]ntro$".r,
  "(?:\\(.*\\) )?\\w+ \"(.*)\" \\([Ff]rom ".r,
  "^[Ff]rom (\\w+ \\w+ #[0-9]+)$".r,
  "^[Ff]or \\w+ \"(.*)\" /".r,
  "[Ee]ndpart [Oo]f .* \"(.*)\"".r,
  "[Uu]sed [Ii]n \"(.*?)\"".r, 
  "[Ff]or (\\w+-\\w+ [0-9]+)".r,
  "^(\\w+) [Ii]ntro-[Tt]une$".r,
  "^[Ff]rom (\\w+)$".r,
  "[Ee]ndpart tune of the (.*) [Dd]emo".r,
  "^[Ff]rom (\\w+ [Tt]unes [0-9]+)".r,
  "[Tt]rack [Ff]rom [Tt]he (.*) [Dd]emo".r,
  "[Uu]sed [Ii]n [Tt]he (.*) [Bb]y \\w+$".r,
  "From \"(.*)\" / .* [Ss]lideshow".r,
  "[Uu]sed [Ii]n \\w+-\\w+ \"(.*)\" /".r,
  "[Uu]sed [Ii]n (\\w+ #[0-9]+) [Ii]ntro".r,
  "[Uu]sed [Ii]n \\w+'s \"(.*)\" [Ii]ntro".r,
  "^[Bb]y .* [Ff]rom: (.*)".r,
  "^[Ff]or \\w+ \"(.*)\"$".r,
  "^\\w+ \"(.*)\" [Dd]emo$".r,
  "^[Ff]or (\\w+ [0-9]+)+$".r,
  "^[Ff]or (\\w+ [0-9]+) - ".r,
  "^[Ff]or (\\w+ [0-9]+)+ \\(PC".r,
  "[Ii]n '\\d{2} [Ff]or \\w+ \"(.*)\"".r,
  "[Bb]y \\w+ '\\d{2} - (.*) [Ii]ntro".r,
  "[F]or .* [Dd]emo \"(.*)\" [Bb]y ".r,
  "[Ii]n \\w+ '\\d{2} [Ff]or (\\w+ [0-9]+)$".r,
  "[Ii]n '\\d{2} [Ff]or (\\w+ \\w+ #[0-9]+) & #[0-9+]".r,
  "^[F]rom (\\w+ \\w+ [0-9]+)$".r,
  "^[F]rom (\\w+ \\w+ [0-9]+) -".r,
  "^[F]rom (\\w+) in ".r,
  "^\\((.*) - Music .*\\)$".r,
  "^\\((.*) - Music .*\\) -".r,
  "[Aa]ka \".*\" - [Ff]rom (.*)".r,
  "[Ff]rom \\w+ \"(.*?)\"".r, // From Crusaders "Bacteria"
  "[Ff]rom \\w+ \\w+ (\".*\" [Ii]ssue #[0-9]+)".r,
  "[Ff]rom \\w+ \\w+ \"(.*?)\"".r,
  "[Ff]rom \\w+-\\w+ \"(.*?)\"".r,
  "[Ff]rom (The \\w+ #[0-9]+) [Ii]ntro$".r,
  "[F]rom \\w+ (MD #[0-9+])".r,
  "[Uu]sed [Ii]n \\w+ (MD #[0-9+])".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2} - [Ff]or \\w+ \"(.*)\"".r,
  "[Ff]rom .* [Aa]sm '\\d{2} \"(.*)\" [Ii]ntro".r,
  "[Uu]sed [Ii]n .* [Dd]emo \"(.*)\"".r,
  "[Uu]sed [Ii]n [Tt]he '(.*)' .* [Mm]usic.*[Dd]isk".r,
  "[Ff]rom \\w+ (\\w+) \\(.*Party".r,
  "^[Ff]rom (\\w+ \\w+) / \\w+$".r,
  "^[Ff]rom (\\w+ [0-9]+)$".r,
  "[Ff]or (\\w+ [Aa]nd \\w+ [0-9]+)$".r,
  "[Ii]n \\w{3} '\\d{2} [Ff]or \"(.*)\"".r,
  "[Uu]sed in (\\w+ #[0-9]+)".r,
  "[Uu]sed in Decnite (\\w+ #[0-9]+)".r,
  "[Uu]sed in (\\w+ \\w+ #[0-9]+)".r,
  "[Uu]sed in \\w+/\\w+ \"(.*)\" [Ii]ntro".r,
  "[Ff]or [Aa] [Dd]emo \\(\"(.*)\"\\)".r,
  "^[Ff]or (\\w+ [0-9]+)$".r,
  "[Ff]rom (\\w+ \\w+ #[0-9]+)".r,
  "[Ee]dition [Ff]or \"(.*)\"".r,
  "^[Ii]n '\\d{2} [Ff]or \"(.*)\"".r,
  "[Ii]n \\w+ '\\d{2} - [Ff]rom (\\w+ #[0-9]+) / ".r,
  "In \\w{3} '\\d{2} [Ff]rom \\w+! \"(.*)\"".r,
  "^\\(.*\\) [Ff]rom (\\w+-\\w+ #[0-9]+)".r,
  "[Ww]ith \\w+ [Ff]rom (\\w+ [0-9]+) [Ii]ntro".r,
  "^\\(.*\\) [Ff]rom (\\w+ [0-9]+)$".r,
  "^[Uu]sed [Ii]n [Tt]he [Gg]ame (\\w+)".r,
  "[Ii]ntro[Pp]art [Ff]rom \\w+ '(.*)' [Dd]emo".r,
  "[Ii]n \\w+/\\w+ '\\d{2} [Ff]or .* '\\d{2} [Dd]emo \"(.*)\"".r,
  "[Uu]sed [Ii]n \\w+ (\\w+-[0-9]+)".r,
  "[Ff]rom \\w+'s .* [Gg]ame \"(.*)\"".r,
  "[Uu]sed [Ii]n (\\w+ \\(PC\\) [Dd]isk[Mm]ag #[0-9]+)".r,
  "[Uu]sed [Ii]n \\w+\\+\\w+ \"(.*)\"".r,
  "[Uu]sed [Ii]n \\w+-\\w+ \"(.*)\"".r,
  "[Ff]rom [Tt]he \\w+ \\w+ \"(.*)\" \\w+ [Dd]emo".r,
  "[Ff]rom (\\w+), [Cc]omposed [Oo]n ".r,
  "[Ff]rom \\w+ \\(PC\\) \"(.*)\" [Ii]n ".r,
  "[Ii]n '\\d{2} - [Uu]sed [Ii]n \\w+-\\w+ \"(.*)\" / ".r,
  "\\(\\w+\\) [Ff]rom (\\w+ [0-9]+) \\(".r,
  "[Ii]n '\\d{2} - (\\w+)/\\w+ [0-9]+ [Cc]hipdisk".r,
  "[Bb]y \\w+ '\\d{2} [Ff]or (\\w+ [Mm]usic-[Dd]isk [Vv]ol.[0-9]+)".r,
  "[Ff]rom (\\w+ \\w+ [0-9]+) \\([Ll]oader\\)".r,
  "[Ww]ith \\w+ [Ff]rom (\\w+-[0-9]+) \\(".r,
  "[Bb]y \\w+ [Ii]n '\\d{2} [Ff]or (\\w+ [0-9]+) [Aa]t ".r,
  "[Bb]y \\w+ & \\w+ [Ff]rom (\\w+) \\w+ [Pp]art$".r,
  "[Bb]y .* - '(.*)' [Mm]usic [Dd]isk".r,
  "- (\\w+ \\w+) [Mm]ega[Dd]emo ".r,
  "[Bb]y \\w+ \\(.*\\) [Ff]rom (\\w+ \\w+ \\w+) [Dd]emo$".r,
  "^[Bb]y \\w+/\\w+, (\\w+ [0-9]+) [Ii]ntro$".r,
  "[Ff]rom \"(.*)\" / ".r,
  "[Ff]rom \"(.*?)\"$".r,
  "[Ff]rom \"(.*?)\" - ".r,
  "[Ff]rom \"(.*?)\" \\(".r,
  "[Ff]rom \"(.*?)\", ".r,
  "^[Bb]y \\w+/\\w+ \\w+ \\(.*\\) [Ff]or \"(.*)\" [Dd]emo$".r,
  "^(\\w+ \\w+)/.*? [Dd]emo".r, // Desert Dream/Kefrens demo 
  "^(\\w+)/.*? [Dd]emo".r, 
  "^(\\w+ \\w+)/.*? [Ii]ntro".r,
  "^(\\w+)/.*? [Ii]ntro".r,
  "^(.*?) [Tt]itle[Tt]rack".r,
  "^(.*?) [Ii]ntro[Tt]une".r,
  "^(\\w+ \\w+) / \\w+$".r,
  "^(\\w+ \\w+)/\\w+$".r,
  "^[Bb]y \\w+ [Ff]or ([A-Za-z]+[0-9]+)$".r,
  "^[Bb]y \\w+ [Ii]n ([A-Za-z]+[0-9]+)-[Ii]ntro [Oo]n ".r,
  "^\\(.*\\) [Bb]y \\w+ [Ff]rom (\\w+) - ".r,
  "^[Bb]y \\w+ '\\d{2} [Ff]or \"(.*)\"$".r,
  "^[Bb]y \\w+ \\w+ [Oo]n \\d{2}-\\w{3}-\\d{2} [Ff]rom (\\w+ [0-9]+)$".r,
  "^[Bb]y \\w+ & \\w+ [Ff]or (\\w+ [0-9]+)$".r,
  "^[Bb]y \\w+ \\w+ - \"(.*)\" [Ee]ndpart$".r,
  "^[Bb]y \\w+/\\w+ [Oo]n \\d{2}-\\w{3}-\\d{2} [Ff]or (\\w+ [0-9]+) ".r,
  "^\\(.*\\) [Bb]y \\w+-\\w+/\\w+ [Ff]rom (\\w+ [0-9]+) [Ii]ntro".r,
  "[Bb]y \\w+ / \\w+ '\\d{2} [Ff]or \\w+ \"(.*)\"".r,
  "^[Bb]y \\w+ [Ff]or \\w+ \"(.*)\"$".r,
  "^[Bb]y \\w+ [Ii]n \\w+ \"(.*)\"$".r,
  "^[Bb]y \\w+ [Ff]rom (\\w+-\\w+)$".r,
  "^[Bb]y \\w+ - [Uu]sed [Ii]n \\w+-\\w+-(\\w+) / ".r,
  "^\\(.*\\) [Bb]y \\w+ [Ff]rom ([A-Za-z\\.]+ #?[0-9]+)$".r,
  "^\\(.*\\) [Bb]y \\w+/\\w+ [Ff]rom \"(.*)\" ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+/\\w+ \\w+ [Ff]or (\\w+-\\w+)$".r,
  "^[Bb]y \\w+ [Ff]rom \\w+.\\w+ \"(.*)\"".r,
  "[Bb]y \\w+/\\w+ [Ff]rom (\\w+ #[0-9]+) [Dd]isk[Mm]ag".r,
  "^[Bb]y \\w+/\\w+ [Ff]rom (\\w+ \\w+ [0-9]+)$".r,
  "^\\(.*\\) [Bb]y \\w+ \\(.*\\) [Ff]rom \\w+ (\\w+-\\w+)$".r,
  "^[Bb]y \\w+ / \\w+ [Ff]rom (\\w+ [0-9]+)$".r,
  "^\\(.*\\) [Bb]y \\w+ \\w+/\\w+ [Ff]rom (\\w+ [0-9]+) \\(".r, 
  "^By \\w+. \\w+ [Ff]or [Tt]he (\\w+-\\w+) [Ii]ntro".r,
  "^[Bb]y \\w+ \\w+ / \\w+ [Ff]or (\\w+)$".r,
  "^[Bb]y \\w+/\\w+ [Ff]rom (\\w+ #?[0-9]+) [Bb]y ".r,
  "^[Bb]y \\w+ [Ff]rom \"(.*)\" [Ss]lide[Ss]how".r,
  "[Ff]rom (\\w+-\\w+-\\w+) [Bb]oot[Bb]lock".r,
  "^[Bb]y \\?\\?\\?/\\w+ [Ff]rom (\\w+ \\w+) [Ll]oader$".r,
)

def parseAlbum(_album: String, _publishers: Buffer[String], comment: String) = {
  var album = _album
  var publishers = _publishers
  var albumPattern = ""
  if (album.isEmpty &&
      !comment.toLowerCase.contains("adapted from ") &&
      !comment.toLowerCase.contains("adaptation from ") &&
      !comment.toLowerCase.contains("arranged from ") &&
      !comment.toLowerCase.contains("from a song") &&
      !comment.toLowerCase.contains("ripped from") &&
      !comment.toLowerCase.startsWith("adapted ") &&
      !comment.toLowerCase.startsWith("converted ") &&
      !comment.toLowerCase.startsWith("remixed ") &&
      !comment.toLowerCase.startsWith("chip-remixed ") &&
      !comment.toLowerCase.startsWith("from the so-called ") &&
      !comment.toLowerCase.startsWith("from the great ") &&
      !comment.toLowerCase.startsWith("with a little help") &&
      !comment.toLowerCase.contains("the hoi")
  ) boundary {
    for (pattern <- albumPatterns) {
      pattern.findFirstMatchIn(comment).foreach { m =>
        album = m.group(1).trim.replace("\"", "")
        albumPattern = pattern.regex
        break()
      }
    }
  }
  album = album.trim

  if (album.isEmpty && publishers.isEmpty && (
    comment.toLowerCase.contains("ram jam charts") ||
    comment.toLowerCase.contains("ramjam charts") ||
    comment.toLowerCase.contains("the charts"))) {
    publishers += "Ram Jam"
    val index = comment.toLowerCase.indexOf("charts")
    album = "The Charts " + comment.substring(index + 7)
      .replaceAll(" \\(.*", "").replaceAll(" [Ii]ntro.*", "").replace("\"","").trim
  }
  (album, albumPattern, publishers)
}

val publisherPatterns = Seq(
  "[Ff]rom the (?:1st)? (.*) AGA [Dd]emo".r,
  "[Ff]rom the (?:2nd)? (.*) AGA [Dd]emo".r,
  "[Ff]rom \".*\" [Bb]y (\\w+)".r, // From "3 from 1" by Chryseis
  "[Ff]rom .*/(.*?) [Dd]emo".r, // From Charles 2000/Banal Projects demo 
  "[Ff]rom (\\w+) .*'\\d{2} [Dd]emo".r, // From JollyWorker Asm'95 Demo
  "[Ff]rom \".*\" / (.*?) [Mm]ag".r, // In '93 from "Outlaw #2" / Eremation mag 
  "[Ff]rom \".*\" / (.*?) [Pp]ack".r, // From "Burning Angels #2" / Symbiosis pack
  "[Ff]rom (.*) \".* [Mm]usic-[Dd]isk".r,
  "[Ff]rom (\\w+) [Mm]usic-[Dd]isk".r,
  "[Ff]rom (\\w+ & \\w+) \"".r, //  From EMF & PLANT "Caero" TP95 Winning Demo (PC)
  "[Bb]y \\w+ / (\\w+) [Ff]rom [Tt]he \"".r,
  "[Ff]rom (\\w+) \"".r,
  "[Ff]rom (\\w+[-| ][0-9]+) \"".r,
  "[Ff]rom (\\w+)'s.* \"".r, // From CNCD's little game "Petterin Reaktiopeli" in Jun '92
  "[Ff]rom [Aa] (.*) [Pp]ack".r, // For a Sanity Pack
  "[Ff]rom [Aa]n (\\w+) [Dd]emo".r,
  "[Ff]rom [Tt]he (.*) [Mm]ega[Dd]emo \"".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo [0-9IiVv]+$".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo$".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo [0-9IiVv]+ \\(".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo \\(".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo [0-9IiVv]+ -".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo -".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo [0-9IiVv]+,".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo,".r,
  "[Ff]rom (.*) [Mm]ega[Dd]emo!".r,
  "[Ff]rom [Th]he \\d{4} (\\w+) [Mm]ega[Dd]emo".r,
  "[Ff]rom \"(\\w+) Megademo\" in".r,
  "[Ff]rom \"(\\w+) Megademo [0-9IiVv]+\" in".r,
  "[Ff]rom .* & (\\w+) \".*\" [Dd]emo".r,
  "[Ff]rom (.*) \".*\".*[Dd]emo".r, 
  "[Ff]rom .* [Bb]y (\\w+ \\w+)$".r,
  "[Uu]sed [Ii]n (\\w+) \"".r, // // Used in Angels "Evolution" coded by Ninja ;-))
  "[Uu]sed [Ii]n (\\w+) [Dd]emo".r, // Used in Dragons Demo for the Gen-4 DemoCompo in May '90
  "[Uu]sed [Ii]n [Bb]oth (.*) \"".r,
  "[Uu]sed in \\w+'s \".*\" [Ii]ntro \\((.*)\\)$".r,
  "[Uu]sed [Ii]n (.*) [Dd]emo \"".r,
  "[Ff]ound [Ii]n (\\w+) \"".r, // Found in Silicon "Birthday" - Hidden Muzak ;-)
  "[Mm]ade [Ff]or (\\w+ \\w+) \"".r,
  "[Mm]ade [Ff]or (\\w+) \"".r,
  "[Ff]or (\\w+)'s [Dd]emo".r, // In '96 for Tran's demo "Luminati" - Great!
  "[Ff]or (\\w+)'s [Ii]ntro".r,
  "[Ff]or (\\w+)'s [Dd]entro".r,
  "[Ff]or [Aa]n (\\w+) [Ii]ntro".r, // For an LSD Intro
  "[Ff]or [Aa] (.*) \".*?\" [Ii]ssue".r, // For a DRD "Live" Issue...
  "[Ff]or [Aa] (.*) [Pp]ack".r, // For a Sanity Pack
  "[Ff]or [Aa] (.*) [Dd]entro".r,
  "[Ff]or [Tt]he (.*) [Mm]ega[Dd]emo \"".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo [0-9IiVv]+$".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo$".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo [0-9IiVv]+ \\(".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo \\(".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo [0-9IiVv]+ -".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo -".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo [0-9IiVv]+,".r,
  "[Ff]or (.*) [Mm]ega[Dd]emo,".r,
  "[Ff]or [Th]he \\d{4} (\\w+) [Mm]ega[Dd]emo".r,
  "[Ff]or (.*) \".*\" in".r,
  "[Ff]or (.*) \".*\" [Dd]entro".r,
  "[Ff]or (.*) \".*\".*[Dd]emo".r,
  "[Ff]or (.*) \".*\" [Ss]lide[Ss]how".r, //  (P60A) For Exceed "Mosaic" SlideShow (TP94)
  "[Ff]or (.*) BBStro".r,
  "[Ff]or .* / (\\w+)$".r,
  "[Ff]or \".*\" [Bb]y (\\w+ \\w+)$".r,
  "[Ff]or \".*\" [Bb]y (\\w+)".r,
  "^/(\\w+ \\w+)$".r, // /Sonic Projects
  "^/(\\w+ [a-zA-Z]{4,}) '".r, // /Sonic Projects '91
  "^/(\\w+) \\w{3} '".r, // /Alchemy Jun '93
  "^/(\\w+ \\w+) [Ff]or".r, // /Sonic Projects for
  "^/(\\w+ \\w+),".r, // /DENS Design, for an issue of D-TECT "Hack Mag"
  "^/(\\w+ \\w+) \\(".r, // /Sonic Projects (...)
  "^/(\\w+)$".r, // /Sanity
  "^/(\\w+) '".r, // /Sanity '91
  "^/(\\w+) [Ff]or".r, // /Sanity for
  "^/(\\w+) [Ff]rom".r, // /Anarchy from Raw 2 in '91 
  "^/(\\w+) [Ii]n".r, // /Delight in Mar '94
  "^/(\\w+) [Oo]n ".r, // /Plague on 03-Oct-91
  "^/(\\w+) \\(".r, // /Sanity (...)
  "^(\\w+) [Ff]or".r, // Sanity for
  "^\\(.*\\) /(\\w+)$".r, // (P61A) /CNCD
  "- \\w+ 64k[o]?/(\\w+) [Ii]ntro".r,
  "^[Bb]y \\w+/(\\w+ \\w+) \\(".r,
  "^\\w+ \\w+/(\\w+) [Dd]emo".r, // Desert Dream/Kefrens demo 
  "^\\w+/(\\w+) [Dd]emo".r,
  "^[Bb]y \\w+/(\\w+) [Ff]or ".r,  
  "^/(\\w+ \\w+) \\w{3} '".r, // /Dual Crew Feb '92
  "^\\w+ \\w+ / (\\w+)$".r,
  "^\\w+ \\w+/(\\w+)$".r,
  "^[Ii]n '\\d{2} \\(/(.*)\\)$".r,
  "^/(\\w+) \\d{1,2}-\\w{3}-(\\d{2,4})$".r,
  "^/(\\w+) -".r,
  "^/(\\w+ \\w+) -".r,
  "^/([Tt]he \\w+ \\w+) '".r,
  "[Uu]sed [Ii]n (\\w+) .* \\(mini\\) Music-Disk".r,
  "[Ff]rom (\\w+) Tune-Disk".r,
  "[Ff]rom .*/(\\w+) [Cc]hipdisk".r,
  "^/(\\w+ \\w+ \\w+)$".r,
  "[Uu]sed [Ii]n [Aa] (\\w+) [Dd]emo -".r,
  "^[Ff]or (\\w+) \".*\" /".r,
  "[Ee]ndpart [Oo]f (.*) \"".r,
  "[Ff]rom the so called (.*) [Dd]emo, [Cc]harted".r,
  "[Ee]ndpart tune of the .* [Dd]emo [Bb]y (.*)$".r,
  "[F]or .* [Ii]ntro [Bb]y (\\w+)$".r,
  "[Tt]rack [Ff]rom [Tt]he .* [Dd]emo [Bb]y (.*)$".r,
  "[Uu]sed [Ii]n [Tt]he .* [Bb]y (\\w+)$".r,
  "[Uu]sed [Ii]n [Aa] (\\w+) [Ii]ntro".r,
  "From \".*\" / (.*) [Ss]lideshow".r,
  "^[Ff]or (\\w+) \".*\"$".r,
  "^(\\w+) \".*\" [Dd]emo$".r,
  "^[Ff]rom \".*\" [Dd]emo [Bb]y (\\w+)".r,
  "^[Ff]or (\\w+)$".r,
  "^[Ff]or (\\w+) - ".r,
  "[Ii]n '\\d{2} [Ff]or (\\w+) \".*\"".r,
  "[F]or .* [Dd]emo \".*\" [Bb]y (\\w+ \\w+)$".r,
  "^/(\\w+.)$".r,
  "^/(\\w+.) -".r,
  "[Bb]y .*, (\\w+.) [Ii]ntro".r,
  "^[Ff]rom [Tt]he (\\w+.) [Dd]emo$".r,
  "From (\\w+) [Ss]lide[Ss]how \"".r,
  "[Ff]rom (\\w+) \"".r, // From Crusaders "Bacteria"
  "[Ff]rom (\\w+ \\w+) \"".r,
  "[Ff]rom (\\w+-\\w+) \"".r,
  "[Ff]rom .* / (\\w+) [Cc]hip[Dd]isk".r,
  "[Uu]sed [Ii]n \".*\" / (\\w+) [Mm]essage[Bb]ox".r,
  "[F]rom (\\w+) MD #".r,
  "[Uu]sed [Ii]n (\\w+) MD #".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2} - [Ff]or (\\w+) \"".r,
  "[Ii]n \\w{3} '.* [Ff]or .* (\\w+) [Dd]entro".r,
  "[Ff]rom (\\w+) [Aa]sm '\\d{2} \"".r,
  "[Uu]sed [Ii]n (\\w+) [Aa]sm '\\d{2}".r,
  "[I]n \\w{3} '.* [Ff]or [Tt]he [Rr]ebirth [Oo]f (\\w+)".r,
  "[Ff]rom (\\w+)+ \\w+ \\(.*Party".r,
  "^[Ff]rom \\w+ \\w+ / (\\w+)$".r,
  "[Ii]n '\\d{2} [Ff]or ([Tt]he \\w+ \\w+)$".r,
  "[Ff]or (\\w+ \\w+) \\w+ '\\d{2} [Ii]ntro".r,
  "[Ff]or (\\w+) .*'\\d{2} [Ii]ntro".r,
  "[Ff]rom (\\w+ \\w+) \\w+ '\\d{2} [Ii]ntro".r,
  "[Ff]rom (\\w+) .*'\\d{2} [Ii]ntro".r,
  "[Rr]emember [Tt]his .* (\\w+) [Dd]emo".r,
  "^[Ff]rom \".*\" / (\\w+)$".r,
  "[Uu]sed in (\\w+)/\\w+ \".*\" [Ii]ntro".r,
  "^\\(.*\\) [Ff]or (\\w+)$".r,
  "[Ff]rom \".*\" / (\\w+-\\w+)".r,
  "In \\w{3} '\\d{2} [Ff]rom (\\w+!) \".*\"".r,
  "^[Ff]rom (\\w+) '.*' [Ii]ntro$".r,
  "^[Ff]rom \".*\" / (\\w+) [Ii]ntro$".r,
  "\\(.*\\) [Ff]rom (\\w+) \\w+ '.*[Ii]ntro$".r,
  "^/(\\w+!\\w+)$".r,
  "[Uu]sed [Ii]n (\\w+) [Pp]arty.*[Dd]emo".r,
  "^[Ff]rom ([a-zA-Z|.]+) [Dd]emo".r,
  "[Ii]ntro[Pp]art [Ff]rom (\\w+) '.*' [Dd]emo".r,
  "[Ff]rom [Aa] (\\w+ \\w+) [Pp]arty-[Ii]nvit".r,
  "[Uu]sed [Ii]n [Aa] (\\w+ \\w+) [Dd]emo".r,
  "^/(\\w+ \\w+) [Oo]n ".r,
  "In \\w+/\\w+ '\\d{2} [Ff]or (\\w+) .*'\\d{2} [Dd]emo \"".r,
  "[Bb]y \\w+ [Ff]rom (\\w+) [Ss]lide[Ss]how".r,
  "^\\(.*\\) /(\\w+ \\w+) '".r,
  "^\\(.*\\) [Ii]ntro .* [Ff]rom [Tt]he .* (\\w+) [Dd]emo$".r,
  "[Uu]sed [Ii]n (\\w+) \\w+-[0-9]+".r,
  "^\\(.*\\) [Ff]rom (\\w+) [Pp]arty.* \".*\" [Ii]ntro".r,
  "[Bb]y \\w+/(\\w+) [Oo]n ".r,
  "[Bb]y \\w+/(\\w+) [Ff]rom \\w+ #[0-9]+ [Dd]isk[Mm]ag".r,
  "[Bb]y \\w+/(.*) \\(PC\\)$".r,
  "[Oo]n \\d{2}-\\w{3}-\\d{2} [Ff]or (\\w+)' .* [Ii]ntro".r,
  "[Ff]or (\\w+) \\(PC\\)$".r,
  "^[Uu]sed [Ii]n (\\w+) [Pp]arty.*[Ii]ntro$".r,
  "^For (\\w+) \\w+ [Oo]n \\d{2}-\\w{3}-\\d{2}$".r,
  "[Uu]sed [Ii]n (\\w+\\+\\w+) \".*\"".r,
  "[Uu]sed [Ii]n (\\w+)-UK \".*\"".r,
  "[Uu]sed in (Decnite) \\w+ #[0-9]+".r,
  "[Ff]rom (\\w+) \\(PC\\) \".*\" [Ii]n ".r,
  "[Ii]n '\\d{2} - [Uu]sed [Ii]n (\\w+-\\w+) \".*\" / ".r,
  "[Ii]n '\\d{2} - \\w+/(\\w+ [0-9]+) [Cc]hipdisk".r,
  "[Ii]n \\w{3} '\\d{2} [Ff]or (\\w+)$".r,
  "[Ff]or (\\w+)'s [Bb]oot[Tt]rack".r,
  "^[Bb]y \\w+ [Oo]f (\\w+)$".r,
  "^[Bb]y \\w+.\\w+ / (\\w+) [Oo]n ".r,
  "^[Ff]rom [Aa] (\\w+) [Cc]rack".r,
  "^[Bb]y [A-Z.]+ [Oo]f (\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+ \\w+)$".r,
  "^[Bb]y \\w+ \\w+/(\\w+)$".r,
  "^[Bb]y \\w+ / (\\w+ \\w+) [Oo]n ".r,
  "^[Bb]y \\w+ [Ff]rom (\\w+ \\w+) [Oo]n ".r,
  "^[Bb]y \\w+ \\w+'\\w+ / (\\w+) '".r,
  "^[Bb]y \\w+/(\\w+), [Ii]ntro ".r,
  "^[Bb]y \\w+ [Oo]f (\\w+) [Oo]n ".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+\\. \\w+ / (\\w+-\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+-\\w+/(\\w+) [Ff]rom ".r,
  "^\\(.*\\) [Bb]y \\w+ / (\\w+) \\(".r,
  "^\\(.*\\) [Bb]y \\w+ / (\\w+)$".r,
  "^[Bb]y \\w+/\\w+ [Ff]rom \\w+ #?[0-9]+ [Bb]y (\\w+)$".r,
  "^[Bb]y \\w+/(\\w+) [Ff]rom ".r,
  "^[Bb]y \\w+/(\\w+) [Ii]n ".r,
  "^[Bb]y \\w+/(\\w+ 1911)$".r,
  "^[Bb]y \\w+/(\\w+) \\d{2}$".r,
  "^[Bb]y \\w+/(\\w+) \\d{4}$".r,
  "^[Bb]y \\w+/(\\w+ \\w+)$".r,
  "^[Bb]y \\w+ \\w+ / (\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+) '".r,
  "^[Bb]y \\w+ \\w+ / (\\w+) '".r,
  "^[Bb]y \\w+ \\w+ \\w+ [Oo]f (\\w+) '".r,
  "^[Bb]y \\w+ \\w+/(\\w+) [Oo]n ".r,
  "[Bb]y \\w+ / \\w+ '\\d{2} [Ff]or (\\w+) \".*\"".r,
  "^\\(.*\\) [Bb]y \\w+ / (\\w+) [Nn]orway".r,
  "^[Bb]y \\w+/(\\w+) '\\d{2}$".r,
  "^[Bb]y \\w+-\\w+ / (\\w+)$".r,
  "^[Bb]y \\w+ / (\\w+!)$".r,
  "^[Bb]y \\w+ / (\\w+ [0-9]+)$".r,
  "^[Bb]y \\w+ / (\\w+ \\w+)$".r,
  "^[Bb]y \\w+/(\\w+), ".r,
  "^[Bb]y \\w+/(\\w+ [A-Za-z0-9\\.]+)$".r,
  "^[Bb]y \\w+ / (\\w+) '".r,
  "^[Bb]y \\w+ [Ff]or (\\w+) \".*\"$".r,
  "^[Bb]y \\w+ / (\\w+) [Ii]n ".r,
  "^[Bb]y \\w+/(\\w+) \\(".r,
  "^[Bb]y \\w+ [Ii]n (\\w+) \".*\"$".r,
  "\".*\" \\w+/(\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+ \\w+)/[A-Za-z0-9\\.]+$".r,
  "^[Bb]y \\w+ / (\\w+) [Ff]or ".r,
  "^[Bb]y \\w+ \\w+ / (\\w+) [Ii]n ".r,
  "^[Bb]y \\w+/(\\w+ \\w+) [Ff]rom \".*\"$".r,
  "^[Bb]y \\w+ / (\\w+) - ".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+)/[A-Z\\.]+$".r,
  "^[Bb]y \\w+/(\\w+)/[A-Z\\.]+$".r,
  "^(?:.* ch - )?[Bb]y \\w+ / (\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+) [Ff]rom \"".r,
  "^[Bb]y \\w+ / (\\w+ \\w+ \\w+)$".r,
  "^[Bb]y \\w+ / (\\w+) [Ff]rom \"".r,
  "^[Bb]y [A-Za-z0-9\\.]+/(\\w+) [Oo]n ".r,
  "^[Bb]y \\w+ - [Uu]sed [Ii]n (\\w+-\\w+)-\\w+ / ".r,
  "^(?:.* ch - )?[Bb]y \\w+ \\w+ / (\\w+)$".r,
  "^[Bb]y \\w+ \\w+ \\w+ / (\\w+)$".r,
  "^[Bb]y \\w+ \\w+ / (\\w+), ".r,
  "^[Bb]y [A-Za-z0-9\\.]+ [Oo]f (\\w+) [Oo]n ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+ \\w+/(\\w+)/\\w+$".r,
  "^[Bb]y \\w+/(\\w+) - ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+ \\w+ \\w+ / (\\w+)$".r,
  "^[Bb]y \\w+ \\w+ [Oo]f (\\w+)$".r,
  "^[Bb]y [A-Za-z0-9\\.]+ [Ff]rom \"\\w+/(\\w+)\"$".r,
  "^(?:.* ch - )?[Bb]y \\w+/(\\w+) - ".r,
  "^[Bb]y \\w+ / (\\w+) \\(".r,
  "^[Bb]y \\w+ / (\\w+ \\w+) [Ii]n ".r,
  "^[Bb]y \\w+ \\w+ & \\w+ [Oo]f (\\w+) [Oo]n ".r,
  "^[Bb]y \\w+/(\\w+ \\w+) [Ii]n ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+ [Ff]rom (\\w+) \\w+-\\w+ [Dd]emo".r,
  "^[Bb]y \\w+ \\w+ & \\w+ [Ff]rom [Aa] (\\w+) [Mm]ag$".r,
  "^(?:.* ch - )?[Bb]y \\w+/(\\w+)$".r,
  "^[Bb]y \\w+ \\w+ / (\\w+ \\w+)$".r,
  "^[Bb]y \\w+ [Ff]rom (\\w+) [Bb][Bb][Ss]tro$".r,
  "^[Bb]y \\w+ [Oo]f (\\w+ \\w+)$".r,
  "^[Bb]y \\w+/(\\w+ \\w+) [Ff]rom \"".r,
  "^[Bb]y \\w+ \\(.*\\) / (\\w+) [Ff]rom ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+/(\\w+ \\w+) [Ff]or ".r,
  "^(?:\\(.*\\) )?(\\w+) \"\\w+\" \\([Ff]rom ".r,
  "^[Bb]y \\w+ [Oo]f (\\w+) - ".r,
  "^(?:\\(.*\\) )?[Bb]y \\w+ [Ff]or (\\w+) [Ss]lide[Ss]how".r,
  "^[Bb]y \\w+ [Ff]rom (\\w+.\\w+) \"".r,
  "[Bb]y \\w+/(\\w+) [Ff]rom \\w+ #[0-9]+ [Dd]isk[Mm]ag".r,
  "^(?:.* ch - )?[Bb]y \\w+/(\\w+) \\(.*\\) [Oo]n ".r,
  "^[Bb]y \\w+/(\\w+)! [Ff]rom ".r,
  "^\\(.*\\) [Bb]y \\w+ \\(.*\\) [Ff]rom (\\w+) \\w+-\\w+$".r,
  "^[Bb]y [A-Za-z0-9\\.]+ / (\\w+)$".r,
  "^[Bb]y \\w+ / (\\w+) , ".r,
  "^[Bb]y [A-Za-z]'\\w+/(\\w+)$".r,
  "^[Bb]y \\w+ / (\\w+) [Ff]rom ".r,
  "^\\(.*\\) [Bb]y \\w+/(\\w+ \\w+ \\w+) [Ff]rom \"".r,
  "^\\(.*\\) [Bb]y \\w+ \\w+/(\\w+) [Ff]rom ".r,
  "^\\(.*\\) [Bb]y \\w+-\\w+/(\\w+)/\\w+$".r,
  "^[Bb]y \\w+/(\\w+) \\w{2} \\d{2}$".r,
  "^(?:.* ch - )?[Bb]y \\w+/(\\w+) [Ii]n ".r,
  "^[Bb]y \\w+ \\w+ \\w+ / (\\w+) [Oo]n ".r,
  "^[Ff]rom [Aa] (\\w+) [Dd]emo, ".r,
  "^\\(.*\\) [Bb]y \\w+ / (\\w+) [Oo]n ".r,
  "^[Bb]y \\w+ & \\w+ [Oo]f (\\w+)$".r,
  "^\\(.*\\) [Bb]y \\w+ / (\\w+) [Ff]rom \"".r,
  "^[Bb]y \\w+ & \\w+ / (\\w+)$".r,
  "^[Bb]y \\w+ [Oo]f (\\w+) '".r,
  "^\\w+! [Bb]y \\w+/(\\w+)! ".r,
  "^[Bb]y \\w+ \\w+ / (\\w+) [Ff]or ".r,
  "^[Bb]y \\w+ / (\\w+ \\w+) / \\w{2}$".r,
  "^[Bb]y \\w+ \\w+ / (\\w+ \\w+ \\w+) [Ii]n '".r,
  "^[Bb]y \\w+-\\w+ / (\\w+) \\(\\w+\\)$".r,
  "^[Bb]y \\w+ / (\\w+ \\w+) '\\d{2} \\(\\w+\\)$".r,
  "^[Bb]y \\w+/(\\w+) '\\d{2} \\(".r,
  "^[Bb]y [A-Za-z0-9\\.]+ [Oo]f ([A-Za-z0-9\\.]+)$".r,
  "^[Bb]y \\w+-\\w+/(\\w+), ".r,
  "^[Bb]y \\w+/([A-Za-z0-9\\.]+) !".r,
  "^[Bb]y \\w+ & \\w+ [Oo]f (\\w+) [Oo]n ".r,
  "^[Bb]y \\w+ [Oo]f (\\w+) \\(".r,
  "^[Bb]y \\w+ / (\\w+ \\w+) !$".r,
  "^[Bb]y \\w+ [Oo]f (\\w+) [Ii]n ".r,
  "^[Bb]y \\w+ / (\\w+), ".r,
  "^[Bb]y \\w+/(\\w+-\\w+)$".r,
  "^[Bb]y \\w+ [Ii]n (\\w+-\\w+) \"".r,
  "^[Bb]y \\w+ \\w+ [Oo]f (\\w+ & \\w+) [Ff]rom ".r,
  "^[Bb]y \\w+/(\\w+ \\w+ \\w+)!$".r,
  "^[Ff]rom (\\w+ \\w+ \\w+) [Dd]emo '".r,
  "^[Bb]y \\w+ [Ff]rom \".*\" / (\\w+ \\w+)$".r,
  "^[Bb]y [A-Za-z0-9\\.]+ / (\\w+) '".r,
  "^[Bb]y \\w+ [Oo]f (\\w+) [A-Z]+ '".r,
  "^[Bb]y \\w+ \\w+ [Ff]rom [Aa]n (\\w+ \\w+) [Dd]emo$".r,
  "^[Bb]y \\w+ / (\\w+ \\w+) \\(.*\\)$".r,
  "^[Bb]y \\w+ [Oo]f (\\w+ \\w+) [Oo]n ".r,
  "^[Bb]y \\w+ [Ff]rom (\\w+ \\w+) [Mm]ega[Dd]emoo".r,
  "^[Bb]y \\w+ \\w+ / (\\w+ \\w+) [Ff]rom ".r,
  "^[Bb]y \\w+/(\\w+!) '".r,
  "^\\w+ \\w+/(.*?) [Ii]ntro$".r, 
  "^\\w+/(.*?) [Ii]ntro".r,
  "^(\\w+) \".*\"".r,
  "^[Bb]y \\?\\?\\?/(\\w+) [Ff]rom \\w+ \\w+ [Ll]oader$".r,
  "^[Bb]y \\w+. / (\\w+) - ".r,
)

def parsePublishers(_publishers: Buffer[String], comment: String) = {
  var publishers = _publishers
  var publisherPattern = ""
  if (!comment.toLowerCase.startsWith("adapted ") &&
      !comment.toLowerCase.startsWith("converted ") &&
      !comment.toLowerCase.startsWith("remixed ") &&
      !comment.toLowerCase.startsWith("chip-remixed ") &&
      !comment.toLowerCase.startsWith("from the so-called ") &&
      !comment.toLowerCase.startsWith("from the great ") &&
      !comment.toLowerCase.startsWith("with ") &&
      !comment.toLowerCase.contains("adapted from ") &&
      !comment.toLowerCase.contains("arranged from ") &&
      !comment.toLowerCase.contains("ripped from ") &&
      !comment.toLowerCase.contains("analogy's hi-tech party") &&
      !comment.toLowerCase.contains("from a song") &&
      !comment.toLowerCase.contains("the hoi")
  ) boundary {
    for (pattern <- publisherPatterns) {
      pattern.findFirstMatchIn(comment).foreach { m =>
        val publisher = m.group(1).trim
        if (publisher.nonEmpty)
          publishers = Buffer(publisher.trim.replace("\"", ""))
        //else println(s"Skipping publisher: $publisher")
        publisherPattern = pattern.regex
        break()
      }
    }
  }
  (publishers, publisherPattern)
}

def normalizePublishers(publishers: Buffer[String], authors: Buffer[String], year: Option[Int]) =
  publishers.flatMap(p =>
    if (p.contains(" & ")) p.split(" & ").sorted
    if (p.contains("+")) p.split("\\+").sorted
    else Some(p)
  )
  .map(_.trim)
  .map(_.replace(" (PC)", ""))
  .map(_.replaceAll("^2000ad$", "2000 A.D."))
  .map(_.replaceAll("^2000 a.d.$", "2000 A.D."))
  .map(_.replaceAll("^2000 AD$", "2000 A.D."))
  .map(_.replaceAll("^Absolute$", "Absolute!"))
  .map(_.replaceAll("^ABYSS$", "Abyss"))
  .map(_.replaceAll("^APOLOGY$", "Apology"))
  .map(_.replaceAll("^ANALOG$", "Analog"))
  .map(_.replaceAll("^ASYLUM$", "Asylum"))
  .map(_.replaceAll("^ATM$", "Anathema"))
  .map(_.replaceAll("^ATZ$", "Alcatraz"))
  .map(_.replaceAll("^Atz$", "Alcatraz"))
  .map(_.replaceAll("^AVALON$", "Avalon"))
  .map(_.replaceAll("^AXIS$", "Axis"))
  .map(_.replaceAll("^BOMB$", "Bomb"))
  .map(_.replaceAll("^BP!$", "Birdhouse Projects"))
  .map(_.replaceAll("^C-LOUS$", "C-Lous"))
  .map(_.replaceAll("^Contras$", "Contraz"))
  .map(_.replaceAll("^CRB$", "Cryptoburners"))
  .map(_.replaceAll("^D-TECT$", "D-Tect"))
  .map(_.replaceAll("^DAI$", "Digital Artists Inc."))
  .map(_.replaceAll("^DCS$", "Dual Crew Shining"))
  .map(_.replaceAll("^DDG$", "Digitize Design Group"))
  .map(_.replaceAll("^DEATHSTAR$", "Deathstar"))
  .map(_.replaceAll("^DeathStar$", "Deathstar"))
  .map(_.replaceAll("^DELIGHT$", "Delight"))
  .map(_.replaceAll("^Dens Design", "DENS Design"))
  .map(_.replaceAll("^DEPTH$", "Depth"))
  .map(_.replaceAll("^DIGITAL$", "Digital"))  
  .map(_.replaceAll("^DIONYSUS$", "Dionysus"))
  .map(_.replaceAll("^DMACON$", "Dmacon"))
  .map(_.replaceAll("^DNT$", "Domination"))
  .map(_.replaceAll("^D\\.O\\.C\\.$", "Doctor Mabuse Orgasm Crackings"))
  .map(_.replaceAll("^DOC$", "Doctor Mabuse Orgasm Crackings"))
  .map(_.replaceAll("^DRD$", "Dreamdealers"))
  .map(_.replaceAll("^Dsr$", "Desire"))
  .map(_.replaceAll("^EKO$", "Extremely Kriminal Organisation"))
  .map(_.replaceAll("^EMF$", "Electromotive Force"))
  .map(_.replaceAll("^EOC 1999$", "End Of Century 1999"))
  .map(_.replaceAll("^FC$", "Future Crew"))
  .map(_.replaceAll("^FLT$", "Fairlight"))
  .map(_.replaceAll("^GCI$", "Genocide"))
  .map(_.replaceAll("^GODS$", "Gods"))
  .map(_.replaceAll("^HALFBRAINS$", "Half Brains Team"))
  .map(_.replaceAll("^HMD$", "Hemoroids"))
  .map(_.replaceAll("^Img$", "Imagine"))
  .map(_.replaceAll("^Impact$", "Impact DK"))
  .map(_.replaceAll("^IVORY$", "Ivory"))
  .map(_.replaceAll("^Jormas$", "dA JoRMaS"))
  .map(_.replaceAll("^LIMBO$", "Limbo"))
  .map(_.replaceAll("^LLFB$", "Lloret Free Byte"))
  .map(_.replaceAll("^LOD$", "Lod Production"))
  .map(_.replaceAll("^M12$", "Majic 12")) 
  .map(_.replaceAll("^MAJIC-12$", "Majic 12")) 
  .map(_.replaceAll("^MELON$", "Melon"))
  .map(_.replaceAll("^M.O.N.$", "Maniacs of Noise"))
  .map(_.replaceAll("^MON$", "Maniacs of Noise"))
  .map(_.replaceAll("^MTX$", "Matrix"))
  .map(_.replaceAll("^NGC$", "New Generation Crew"))
  .map(_.replaceAll("^N[Xx][Ss]$", "Noxious"))
  .map(_.replaceAll("^OXYRON$", "Oxyron"))
  .map(_.replaceAll("^PARADISE$", "Paradise"))
  .map(_.replaceAll("^PERSEUS$", "Perseus"))
  .map(_.replaceAll("^Pfy$", "Profecy"))
  .map(_.replaceAll("^PEN$", "Pentagon"))
  .map(_.replaceAll("^PGN$", "Paragon"))
  .map(_.replaceAll("^PHA$", "Phenomena"))
  .map(_.replaceAll("^PHluid$", "pHluid"))
  .map(_.replaceAll("^PLANT$", "Plant"))
  .map(_.replaceAll("^PLS$", "Polaris"))
  .map(_.replaceAll("^PMC$", "Pure Metal Coders"))
  .map(_.replaceAll("^PoP$", "Priests Of Power"))
  .map(_.replaceAll("^RAF$", "Royal Amiga Force"))
  .map(_.replaceAll("^RAZOR$", "Razor 1911"))
  .map(_.replaceAll("^Razor$", "Razor 1911"))
  .map(_.replaceAll("^RAM JAM$", "Ram Jam"))
  .map(_.replaceAll("^RAMJAM$", "Ram Jam"))
  .map(_.replaceAll("^RamJam$", "Ram Jam"))
  .map(_.replaceAll("^Red Sector$", "Red Sector Inc."))
  .map(_.replaceAll("^REM$", "Radical Elite Movement"))
  .map(_.replaceAll("^RSI$", "Red Sector Inc."))
  .map(_.replaceAll("^SAE$", "Share and Enjoy"))
  .map(_.replaceAll("^Share & Enjoy$", "Share and Enjoy"))
  .map(_.replaceAll("^Saf$", "Soldiers And Flowers"))
  .map(_.replaceAll("^SAGAZITY$", "Sagazity"))
  .map(_.replaceAll("^S!P$", "Surprise! Productions"))
  .map(_.replaceAll("^Surprise!Productions$", "Surprise! Productions"))
  .map(_.replaceAll("^Suprise Prods$", "Surprise! Productions"))
  .map(_.replaceAll("^SiH$", "Somewhere In Holland"))
  .map(_.replaceAll("^SCOOP$", "Scoop"))
  .map(_.replaceAll("^SONIC$", "Sonic"))
  .map(_.replaceAll("^SONIK$", "Sonik Clique"))
  .map(_.replaceAll("^SPASM$", "Spasm"))
  .map(_.replaceAll("^SR$", "Jetset"))
  .map(_.replaceAll("^SRT$", "Sector T"))
  .map(_.replaceAll("^SUBMISSION$", "Submission"))
  .map(_.replaceAll("^SYMBIOSIS$", "Symbiosis"))
  .map(_.replaceAll("^T.L.H.$", "The Lazer Heads"))
  .map(_.replaceAll("^TBL$", "The Black Lotus"))
  .map(_.replaceAll("^TCM$", "Teenage Crime"))
  .map(_.replaceAll("^TDD$", "The Dark Demon"))
  .map(_.replaceAll("^Dark Demon$", "The Dark Demon"))
  .map(_.replaceAll("^TEK$", "The Electronic Knights"))
  .map(_.replaceAll("^TFP$", "Trackers Fun Productions"))
  .map(_.replaceAll("^TGT$", "The Geneva Team"))
  .map(_.replaceAll("^TJC$", "The Jungle Command"))
  .map(_.replaceAll("^TLOTB$", "The Lords Of The Bits"))
  .map(_.replaceAll("^TPB$", "The Party Bit"))
  .map(_.replaceAll("^TPPI$", "Tibetan Peach Pie Inc."))
  .map(_.replaceAll("^TRIBE$", "Tribe"))
  .map(_.replaceAll("^TRSI$", "Tristar and Red Sector Inc."))
  .map(_.replaceAll("^TSB$", "The Special Brothers"))
  .map(_.replaceAll("^TSD$", "The Secret Drawer"))
  .map(_.replaceAll("^TSL$", "The Silents"))
  .map(_.replaceAll("^TT$", "Total Techno"))
  .map(_.replaceAll("^US$", "Universal Soldiers"))
  .map(_.replaceAll("^VD$", "Virtual Dreams"))
  .map(_.replaceAll("^VD-FLT$", "Virtual Dreams"))
  .map(_.replaceAll("^VEGA$", "Vega"))
  .map(_.replaceAll("^UNION$", "Union"))
  .map(_.replaceAll("^ZOO$", "Zoo"))
  .map(_.replaceAll("^Melon Dezign$", "Melon"))
  .map(_.replaceAll("^Melon-Dezign$", "Melon"))
  .map(_.replaceAll("^NooON$", "NoooN"))
  .map(_.replaceAll("^PHUTURE 303$", "Phuture 303"))
  .map(_.replaceAll("Polka.*Bros", "Polka Brothers"))
  .map(_.replaceAll("^Silents$", "The Silents"))
  .map(_.replaceAll("^Silent$", "The Silents"))
  .map(_.replaceAll("^SpaceBalls$", "Spaceballs"))
  .map(_.replaceAll("^OBProd$", "Bomb & Oxygene"))
  .map(_.replaceAll("^Impact Studio$", "Impact Studios"))
  .map(_.replaceAll("^Thomas Landspurg$", "TomSoft"))
  .map(_.replaceAll("^[Tt]he [Ee]uro[Cc]harts", "Eurocharts"))
  .map(_.replaceAll("One-Sample-Module", "'1-sample'"))
  .map(_.replaceAll("^[Tt]he [Ff]ud[Gg]e [Dd]emo$", "Fudge"))
  .map(_.replaceAll("'\\d{2}$", ""))
  .map(_.replaceAll(" Sept$", ""))
  .map(_.replaceAll(" UK$", ""))
  .map(_.replaceAll(" sf$", ""))
  .map(_.replace(" from a PC", ""))
  .filterNot(_.toLowerCase == "the")
  .filterNot(_.toLowerCase == "an")
  .filterNot(_.toLowerCase == "a")
  .filterNot(_.toLowerCase == "from")
  .filterNot(_.toLowerCase == "for")
  .filterNot(_.toLowerCase == "aka")
  .filterNot(_.toLowerCase == "kinda")
  .filterNot(_.toLowerCase == "called")
  .filterNot(_.toLowerCase == "composed")
  .filterNot(_.toLowerCase == "made")
  .filterNot(_.toLowerCase == "norway")
  .filterNot(_.toLowerCase == "germany")
  .filterNot(_.toLowerCase == "maxx")
  .filterNot(_.contains("#"))
  .filterNot(_ == "Pixie")
  .filterNot(_ == "Boomer")
  .filterNot(_.toLowerCase == "the intro")
  .filterNot(_.toLowerCase == "the game")
  .filterNot(_.toLowerCase == "the pc")
  .filterNot(_.matches("^[0-9]+$"))
  .filterNot(_.matches("^\\w+ [Ff]rom \\w+$"))
  .filterNot(_ == authors.headOption.getOrElse(""))
  .map(_.trim)
  .flatMap(p =>
    if (p.toLowerCase == "asm") Some("Assembly")
    else if (p.toLowerCase == "nijmegen") {
      if (year.getOrElse(0) == 1993) Some("Somewhere In Holland")
      else if (year.getOrElse(0) == 1994) Some("Bizarre")
      else None
    } else if (p.contains(" & ")) p.split(" & ").sorted
    else if (p == "Impact DK" && authors.contains("Moby")) Some("Impact Inc.")
    else Some(p)
  ).distinct

def parsePartyYear(_year: Option[Int], comment: String) = {
  var year = _year
  if (!year.isDefined && (comment.toLowerCase.contains(" the party ") || comment.toLowerCase.contains(" party-")) &&
      !comment.toLowerCase.contains(" report")
  ) {
    year = comment.toLowerCase.indexOf(" the party ") match {
      case v if comment.toLowerCase.contains("party v") => Some(1995)
      case iv if comment.toLowerCase.contains("party iv") => Some(1994)
      case iii if comment.toLowerCase.contains("party iii") => Some(1993)
      case ii if comment.toLowerCase.contains("party ii") => Some(1992)
      case i if comment.toLowerCase.contains("party i") => Some(1991)
      case y if y > 0 => comment.substring(y + 11, y + 12).toIntOption.map(_ + 1990)
      case _ => comment.toLowerCase.indexOf(" party-") match {
        case y if y > 0 => comment.substring(y + 7, y + 8).toIntOption.map(_ + 1990)
        case _ => year
      }
    }
  }
  year
}

def parseParty(_album: String, publishers: Buffer[String], _publisherPattern: String, _year: Option[Int], comment: String)
  : (String, Buffer[String], String, Option[Int]) = {
  var album = _album
  var album_ = " "
  var year = _year
  var publisherPattern = _publisherPattern
  if (publishers.isEmpty && !comment.toLowerCase.contains(" demo ") && !comment.toLowerCase.contains(" intro ") &&
    !comment.toLowerCase.contains("after the ") && !comment.toLowerCase.contains(" report") &&
    !comment.toLowerCase.contains("planned to be ") &&
  (
    (comment.toLowerCase.contains("compo") && !comment.toLowerCase.contains("compose")) ||
    comment.toLowerCase.contains("charted") || comment.toLowerCase.contains("disqualified") ||
    comment.toLowerCase.contains("unselected") || comment.toLowerCase.contains(" party ") ||
    comment.toLowerCase.contains("gathering") || comment.toLowerCase.contains("saturne") ||
    comment.toLowerCase.contains("gasp") || comment.toLowerCase.contains("3s party") ||
    comment.toLowerCase.contains("trackering ") || comment.toLowerCase.contains("assembly") ||
    comment.toLowerCase.contains("happening") || comment.toLowerCase.contains("rendez-vous") ||
    comment.toLowerCase.contains("tcc party") || comment.toLowerCase.contains("amegaparty") ||
    comment.toLowerCase.contains("society party") || comment.toLowerCase.contains("digital party") ||
    comment.toLowerCase.contains("at hurricane '") || comment.toLowerCase.contains(" the symposium ") ||
    comment.toLowerCase.matches(".*68.*convention.*") || comment.contains(" NAID ") ||
    comment.contains("IRIS NYC") || comment.contains("The NYC") || comment.toLowerCase.contains(" asm '") ||
    comment.toLowerCase.contains("cyclone party") || comment.toLowerCase.contains("south sealand") ||
    comment.toLowerCase.contains("music contest") || comment.toLowerCase.contains("socad") ||
    comment.toLowerCase.contains("rainbow party") || comment.toLowerCase.contains("the gathering") ||
    comment.toLowerCase.contains("one-sample-module") || comment.toLowerCase.contains("doom's day") ||
    comment.toLowerCase.contains("virtual party") || comment.toLowerCase.contains("razor party") ||
    comment.toLowerCase.contains("hackerrence") || comment.toLowerCase.contains("1-sample") ||
    comment.toLowerCase.contains("revenge party") || comment.toLowerCase.contains("the wired '") ||
    comment.toLowerCase.contains("amiga convention") || comment.toLowerCase.contains("the hurricane") ||
    comment.toLowerCase.contains("icing ") || comment.toLowerCase.contains("kindergarden ") ||
    comment.toLowerCase.contains("tcc '93") || comment.toLowerCase.contains("primavera") ||
    comment.toLowerCase.contains("analog party") || comment.toLowerCase.contains("place 2 be") ||
    comment.toLowerCase.contains("silly whinnings") || comment.toLowerCase.contains("summit '") ||
    comment.contains("WOC '") || comment.toLowerCase.contains("southsealand") || comment.toLowerCase.contains("one-sample-mods") ||
    comment.matches(".*[Ff]or [Tt][Gg]\\d{2}.*") || comment.matches(".*[Tt][Pp]\\d{2} [Ee]ntry.*")
  )) { comment match {
    case theparty if comment.toLowerCase.contains(" the party ")
                  || comment.matches(".*[Tt][Pp]\\d{2} [Ee]ntry.*") =>
      publishers += "The Party"

    case thegathering if comment.toLowerCase.contains(" the gathering ")
                      || comment.matches(".*[Ff]or [Tt][Gg]\\d{2}.*") =>
      publishers += "The Gathering"

    case saturne if comment.toLowerCase.contains(" saturne party ")
                 || comment.matches(".*[Ss]aturne [0-9].*" )=>
      publishers += "Saturne Party"
      year = comment.toLowerCase.match {
        case y if y.matches(".*saturne party 1.*") => year.orElse(Some(1993))
        case y if y.matches(".*saturne party 2.*") => year.orElse(Some(1994))
        case _ => year
      }

    case gasp if comment.toLowerCase.contains(" gasp") =>
      publishers += "GASP"
      year = year.orElse(Some(1995))

    case sss if comment.contains("3S Party") =>
      publishers += "3S Party"

    case chipcompo if comment.toLowerCase.contains("20 minute chip") =>
      publishers += chipcompo.replaceAll(".*[Ff]or [Tt]he ", "").replaceAll(" #.*", "").trim

    case trackering if comment.toLowerCase.matches(".*trackering #?[0-9]+.*") =>
      publishers += "The Trackering"
      year = comment.toLowerCase match {
        case y if y.matches(".*trackering #?[1-2].*") => year.orElse(Some(1995))
        case y if y.matches(".*trackering #?[3-9].*") => year.orElse(Some(1996))
        case y if y.matches(".*trackering #?1[0-9].*") => year.orElse(Some(1996))
        case y if y.matches(".*trackering #?2[0-7].*") => year.orElse(Some(1996))
        case y if y.matches(".*trackering #?2[8-9].*") => year.orElse(Some(1997))
        case y if y.matches(".*trackering #?3[0-1].*") => year.orElse(Some(1997))
        case _ => year
      }
    
    case assembly if comment.toLowerCase.contains("assembly '")
                  || comment.toLowerCase.contains("asm '") =>
      publishers += "Assembly"

    case happening if comment.toLowerCase.contains("happening") =>
      publishers += "The Happening"

    case rendezvous if comment.toLowerCase.contains("rendez-vous") =>
      publishers += "Rendezvous"
      year = year.orElse(Some(1992))
    
    case tcc if comment.toLowerCase.contains("tcc party")
                || comment.toLowerCase.contains("tcc '") =>
      publishers += "The Computer Crossroads"
      year = year.orElse(Some(1993))

    case pha if comment.toLowerCase.contains("phenomena easter party") =>
      publishers += "Light and Phenomena Easter Party"
      year = year.orElse(Some(1992))

    case hitech if comment.toLowerCase.contains("hi-tech party") =>
      publishers += "Analogy Hitech Party"
      year = year.orElse(Some(1992))
    
    case amega if comment.toLowerCase.contains("amegaparty") =>
      publishers += "Amega Party"
      year = year.orElse(Some(1991))
    
    case society if comment.toLowerCase.contains("society party") =>
      publishers += "Society Summer Party"
      year = year.orElse(Some(1991))
    
    case digital if comment.toLowerCase.contains("digital party")
                 || comment.toLowerCase.contains(" the symposium ") =>
      publishers += "Digital Symposium"
    
    case hurricane if comment.toLowerCase.contains("hurricane ") =>
      publishers += "Hurricane and Brutal Summer Party"
      year = year.orElse(Some(1992))
    
    case convention68 if comment.toLowerCase.matches(".*68.*convention.*") =>
      publishers += "680xx Convention"
      year = year.orElse(Some(1993))
    
    case naid if comment.contains(" NAID ") =>
      publishers += "NAID"

    case irisnyc if comment.contains("IRIS NYC")
                 || comment.contains("The NYC") =>
      publishers += "Iris New Year Conference"
      year = Some(1991)
    
    case easterconference if comment.toLowerCase.contains("trsi easter party") =>
      publishers += "Easter Conference"
      year = year.orElse(Some(1992))
    
    case cyclone if comment.toLowerCase.contains("cyclone party") =>
      publishers += "Cyclone Party"
      year = year.orElse(Some(1991))
    
    case southsealand if comment.toLowerCase.contains("south sealand")
                      || comment.toLowerCase.contains("southsealand") =>
      publishers += "South Sealand"
    
    case amigacity if comment.toLowerCase.contains("amiga-city") =>
      publishers += "Amiga City"

    case musiccontest if comment.toLowerCase.contains("music contest") =>
      publishers += "Music Contest"
    
    case socad if comment.toLowerCase.contains("socad") =>
      publishers += "SoCal"
      year = year.orElse(Some(1995))

    case hardcoreheaven if comment.toLowerCase.contains("hardcore-heaven") =>
      publishers += "Hardcore Heaven"
      year = year.orElse(Some(1992))

    case rainbowparty if comment.toLowerCase.contains("rainbow party") =>
      publishers += "Radwar Party VII"
      year = year.orElse(Some(1994))
    
    case nexus if comment.toLowerCase.contains("nexus party") =>
      publishers += "Nexus"
      year = year.orElse(Some(1995))
    
    case onesample if comment.toLowerCase.contains("one-sample-module")
                   || comment.toLowerCase.contains("one-sample-mods")
                   || comment.toLowerCase.contains("1-sample") =>
      publishers += "Eurocharts '1-sample'"
      year = year.orElse(Some(1992))
    
    case doomsday if comment.toLowerCase.contains("doom's day")
                  || comment.toLowerCase.contains("doomsday ") =>
      publishers += "Doom's Day Party"
      year = year.orElse(Some(1994))
    
    case ecc if comment.toLowerCase.contains("virtual party") =>
      publishers += "European Computer Conference"
      year = year.orElse(Some(1993))
    
    case razor if comment.toLowerCase.contains("razor party") =>
      if (comment.toLowerCase.contains("in '91")) {
        publishers += "Razor 1911 & Imp-666 Amiga Conference"
        year = year.orElse(Some(1991))
      } else if (comment.toLowerCase.contains("in '92")) {
        publishers += "Rendezvous"
        year = Some(1992)
      }

    case hackerence if comment.toLowerCase.contains("hackerrence") =>
      publishers += "Hackerence"
    
    case revenge if comment.toLowerCase.contains("revenge party") =>
      publishers += "Magnetic Fields Revenge Party"
      year = year.orElse(Some(1990))

    case wired if comment.toLowerCase.contains("the wired '") =>
      publishers += "Wired"
    
    case woc if comment.contains("WOC '") =>
      publishers += "World of Commodore"
    
    case amigaconvention if comment.toLowerCase.contains("amiga convention") =>
      publishers += "Amiga Convention"
  
    case icing if comment.toLowerCase.contains("icing ") =>
      publishers += "Icing"
    
    case kindergarden if comment.toLowerCase.contains("kindergarden ") =>
      publishers += "Kindergarden"
      year = comment.toLowerCase.match {
        case y if comment.toLowerCase.contains("kindergarden 5") => year.orElse(Some(1996))
        case _ => year
      }
    
    case primavera if comment.toLowerCase.contains("primavera") =>
      publishers += "Primavera Party"

    case analog if comment.toLowerCase.contains("analog party") =>
      publishers += "Analog Party"
      year = year.orElse(Some(1994))

    case place2be if comment.toLowerCase.contains("place 2 be") =>
      publishers += "Place To Be"
      year = comment.toLowerCase.match {
        case y if comment.toLowerCase.matches(".*place 2 be .* 1.*") => year.orElse(Some(1993))
        case y if comment.toLowerCase.matches(".*place 2 be .* 2.*") => year.orElse(Some(1994))
        case y if comment.toLowerCase.matches(".*place 2 be .* 3.*") => year.orElse(Some(1995))
        case y if comment.toLowerCase.matches(".*place 2 be .* 4.*") => year.orElse(Some(1996))
        case _ => year
      }
    
    case sillywhinings if comment.toLowerCase.contains("silly whinnings") =>
      publishers += "Ruskean Reian Ritarit"
      album_ = "Silly Whinings"
      year = year.orElse(Some(1994))
    
    case summit if comment.toLowerCase.contains("summit '") =>
      publishers += "Amiga Summit Convention"
      year = year.orElse(Some(1991))

    case _ => boundary {
      val patterns = Seq(
        "[Ff]or [Th]he (.*?) [Rr]eal[Tt]ime.*[Cc]ompo".r,
        "[Ff]or [Th]he (.*?) [Mm]usi.*[Cc]ompo".r,
        "[Ff]or [Th]he (.*?) [Tt]ekno.*[Cc]ompo".r,
        "[Ff]or [Th]he (.*?) .*[Cc]hannel.*[Cc]ompo".r,
        "[Ff]or [Tt]he (.*?) '.* [Cc]hip.*[Cc]ompo".r,
        "[Ff]or [Th]he (.*?) [Cc]ompo$".r,
        "[Ww]on [Th]he (.*?) [Rr]eal[Tt]ime.*[Cc]ompo".r,
        "[Ww]on [Th]he (.*?) [Mm]usi.*[Cc]ompo".r,
        "[Ww]on [Th]he (.*?) [Tt]ekno.*[Cc]ompo".r,
        "[Ww]on [Th]he (.*?) .*[Cc]hannel.*[Cc]ompo".r,
        "[Ww]on [Tt]he (.*?) '.* [Cc]hip.*[Cc]ompo".r,
        "[Ww]on [Th]he (.*?) [Cc]ompo$".r,
        "[Aa]t [Th]he (.*?) [Rr]eal[Tt]ime.*[Cc]ompo".r,
        "[Aa]t [Th]he (.*?) [Mm]usi.*[Cc]ompo".r,
        "[Aa]t [Th]he (.*?) [Tt]ekno.*[Cc]ompo".r,
        "[Aa]t [Th]he (.*?) .*[Cc]hannel.*[Cc]ompo".r,
        "[Aa]t [Tt]he (.*?) '.* [Cc]hip.*[Cc]ompo".r,
        "[Aa]t [Th]he (.*?) [Cc]ompo$".r,
        "[Ff]or (.*?) [Rr]eal[Tt]ime.*[Cc]ompo".r,
        "[Ff]or (.*?) [Mm]usi.*[Cc]ompo".r,
        "[Ff]or (.*?) [Tt]ekno.*[Cc]ompo".r,
        "[Ff]or (.*?) .*[Cc]hannel.*[Cc]ompo".r,
        "[Ff]or (.*?) '.* [Cc]hip.*[Cc]ompo".r,
        "[Ff]or (.*?) [Cc]ompo$".r,
        "[Ff]or [Tt]he (\\w+) '.*[Ch]arted".r,
        "[Cc]harted .* [Aa]t [Tt]he (.*?) '".r,
        "[Cc]harted .* [Aa]t (.*?) [Tt]ekno.*[Cc]ompo".r,
        "[Cc]harted .* [Aa]t (.*?) '".r,
        "[Cc]harted [0-9]+ [Aa]t (\\w+) [Pp]arty".r,
      )
      for (pattern <- patterns) {
        pattern.findFirstMatchIn(comment).foreach { m =>
          val publisher = m.group(1).trim
          if (publisher.nonEmpty && !publishers.contains(publisher) && 
              !publisher.matches("\\d+") && // Skip if just numbers (years)
              publisher.length > 1)   // Skip single characters
            publishers += publisher.trim.replace("\"", "")
          //else println(s"Skipping publisher: $publisher")
          publisherPattern = pattern.regex
          break()
        }
      }
    }
  }
  if (!publishers.isEmpty)
    album = album_
  }
  (album, publishers, publisherPattern, year)
}

def applyQuirks(_authors: Buffer[String], _album: String, _publishers: Buffer[String], _year: Option[Int], comment: String) = {
  var authors = _authors
  var album = _album
  var publishers = _publishers
  var year = _year
  if (year.getOrElse(0) > 1996) {
    year = None
  }
  if (album.startsWith("GODS.")) {
    val split = album.split("[\\.| ]")
    publishers = Buffer(split(0).replace("GODS","Gods").trim)
    album = split.drop(1).mkString(" ").trim
  }
  if (album.startsWith("Crusaders ")) {
    val split = album.split("[\\.| ]")
    publishers = Buffer(split(0).trim)
    album = split.drop(1).mkString(" ").trim
  }
  if (
    (album.toLowerCase.startsWith("with ") && authors.size > 1) ||
    album.toLowerCase == "40k" ||
    album == "Not under 18 years" ||
    album.toLowerCase.startsWith("the so called ") ||
    album.toLowerCase.startsWith("ripp'em all #") ||
    (album.matches("^[Ff]or \\w+$") && comment.toLowerCase.startsWith("for ")) ||
    album.matches("^[0-9]+$") || album.matches("^[Pp]arty [0-9]+$")
  ) {
    album = ""
  }
  if (album.toLowerCase == "bbs") {
    album = ""
    publishers = Buffer.empty
  }
  if (album.startsWith("'") && album.endsWith("'")) {
    album = album.substring(1, album.length - 1)
  }
  if (album.toLowerCase.startsWith("the eurocharts")) {
    album = album.replaceFirst("[Tt]he [Ee]uro[Cc]harts", "Eurocharts").trim
  }
  if (album.toLowerCase.contains("imphobia")) {
    album = album.replaceFirst("\\(PC\\) [Dd]isk[Mm]ag", "").trim
  }
  if (album.matches("^[Rr][Aa][Ww] #.*") || album.matches("^[Rr][Aa][Ww] [0-9]+.*")) {
    album = album.replaceFirst("[Rr][Aa][Ww] ", "R.A.W ").trim
  }
  if (album.matches("^[Rr][Oo][Mm] #.*") || album.matches("^[Rr][Oo][Mm] [0-9]+.*") ||
      album.matches("^[Rr][Oo][Mm]-[0-9]+.*")) {
    album = album.replaceFirst("[Rr][Oo][Mm]", "ROM").trim
  }
  if (album.startsWith("MD #")) {
    album = album.replace("MD #", "Music Disk #")
  }
  if (album == "State of The Art 2") {
    album = "9 Fingers"
  }
  if (album == "Silicon League") {
    album = ""
    publishers = Buffer("Silicon League")
  }
  if (album.startsWith("Vacation #")) {
    publishers = Buffer("Pearl")
  }
  if (album.toLowerCase.startsWith("grapevine ")) {
    publishers = Buffer("LSD")
  }
  if (album.startsWith("ROM ") || album.startsWith("ROM-")) {
    publishers = Buffer("Essence")
  }
  if (album.startsWith("The Charts ")) {
    publishers = Buffer("Ram Jam")
  }
  if (album.startsWith("Imphobia ")) {
    publishers = Buffer("Imphobia")
  }
  if (album.startsWith("Brain Damage ")) {
    publishers = Buffer("Giants")
  }
  if (album.toLowerCase.startsWith("seenpoint ")) {
    publishers = Buffer("Sardonyx")
  }
  if (album.toLowerCase.startsWith("the trackering ")) {
    publishers = Buffer("The Trackering")
    year = album.toLowerCase match {
      case y if y.matches(".*trackering #?[1-2].*") => year.orElse(Some(1995))
      case y if y.matches(".*trackering #?[3-9].*") => year.orElse(Some(1996))
      case y if y.matches(".*trackering #?1[0-9].*") => year.orElse(Some(1996))
      case y if y.matches(".*trackering #?2[0-7].*") => year.orElse(Some(1996))
      case y if y.matches(".*trackering #?2[8-9].*") => year.orElse(Some(1997))
      case y if y.matches(".*trackering #?3[0-1].*") => year.orElse(Some(1997))
      case _ => year
    }
    album = ""
  }
  if (album.isEmpty && publishers.size == 1 && publishers.head == "Viktoria") {
    album = publishers.head
    publishers = Buffer.empty
  }
  if (album == "It's" && publishers.contains("Melon")) {
    album = "Acid Trip"
  }
  if (album == "Mystic" && publishers.size == 1 && publishers.head == "Mystic") {
    album = "The Demo"
  }
  if (authors.size == 1 && authors.head == "MCO" && publishers.size == 1 && publishers.head == "HMC") {
    authors = Buffer("Homicide")
    publishers = Buffer.empty
  }
  if (authors.size == 1 && authors.head == "M.O.N.") {
    authors = Buffer.empty
    publishers = Buffer("Maniacs of Noise")
  }
  if (!authors.isEmpty && album.endsWith(authors.head)) {
    album = ""
    //publishers = Buffer.empty
  }
  authors = authors
    .filterNot(_.toLowerCase == "the")
    .filterNot(_ == "Jimi Hendrix")
    .filterNot(_ == "Skumma Melka")
    .filterNot(_ == "Dr")
    .filterNot(_ == "Mr")
    .filterNot(_ == "DJ")
    .filterNot(_ == "MC")
    .filterNot(a => a != "Q" && a.length <= 1)
    .map(a => if (a == "Danko JR") "Danko" else a)
    .map(a => if (a == "FRED") "Fred" else a)
    .map(a => if (a == "Nerve Axis") "Dvize" else a)
    .map(a => if (a == "Night") "Night of Sounds" else a)
    .map(a => if (a == "Raazzzzzmmmoooooo") "Razmo" else a)
    .map(a => if (a == "Trap Bonzai") "Trap" else a)
  (authors, album, publishers, year)
}

val SEPARATOR = "==============================================================================="

lazy val modsanthology_by_path = sources.modsanthology.groupBy(_.path.split("/").takeRight(2).mkString("/"))

lazy val maz1txt = Paths.get(modsanthology_path + "Mods-1/Lists/Ascii/MAZ1-Authors(A-F).txt").toFile
lazy val maz2txt = Paths.get(modsanthology_path + "Mods-1/Lists/Ascii/MAZ2-Authors(G-Q).txt").toFile
lazy val maz3txt = Paths.get(modsanthology_path + "Mods-1/Lists/Ascii/MAZ3-Authors(R-Z).txt").toFile

def parseMazAuthorsTxt(f: java.io.File) = Using(scala.io.Source.fromFile(f)(scala.io.Codec.ISO8859))(s =>
  val lines = s.getLines
  boundary {
    for (line <- lines) if (line.startsWith("==> Directory of Mods-")) break()
  }
  // parse author abbreviations
  lines.next()
  val authorMap = mutable.Map[String, String]()
  boundary {
    for (line <- lines) {
      if (line.trim.isEmpty) break()
      // 16beat       = 16beat                   < 10>     954k - (Joachim SOPPE / Germany)
      val Array(abbr, name) = line.split("=")
      authorMap += abbr.trim.toLowerCase -> name.split("<")(0).trim.replace("_", " ")
    }
  }
  boundary {
    for (line <- lines) if (line == SEPARATOR) break()
  }
  val metas = Buffer[ModsAnthologyMeta]()
  var authorShort = ""
  var authors = Buffer.empty[String]
  while (lines.hasNext) boundary {
    val line = lines.next().trim
    if (line.startsWith("MAZ")) {
      // MAZ1:16beat
      authorShort = line.split(":")(1)
      val author = authorMap(authorShort.toLowerCase)
      authors =
        if (author.matches(".*,.* and .*")) author.split(" and |, ").map(_.trim).sorted.toBuffer
        else if (author.contains(" & ") && author.length > 5) author.split(" & ").map(_.trim).sorted.toBuffer
        else if (!author.contains(" & ") && author.contains("&") && author.length > 3) author.split("&").map(_.trim).sorted.toBuffer
        else if (author.contains("+") && author.length > 3) author.split("\\+").map(_.trim).sorted.toBuffer
        else if (author.contains("^") && author.length > 3) author.split("\\^").map(_.trim).sorted.toBuffer
        else if (author.contains(" and ")) author.split(" and ").map(_.trim).sorted.toBuffer
        else Buffer(author)
    } else if (line.matches(".*\\..*\\s+[0-9]+.*")) {
      var parts = line.split("\\s+")
      if (parts.length < 2) break()
      val filename = parts(0).trim
      val filesize = parts(1).trim.toInt
      val path = authorShort + "/" + filename
      val md5 = modsanthology_by_path.get(path).headOption.map(_.head.md5)
      if (md5.isEmpty) {
        System.err.println(s"WARN: modsanthology missing md5 for ${path}")
        break()
      }

      val songlength = if (parts.length > 2 && parts(2).startsWith("-")) try {
        val Array(min, sec) = parts(3).drop(1).dropRight(1).split(":")
        parts = parts.drop(5)
        Some(min.toInt * 60 + sec.toInt)
      } catch {
        case _: Exception =>
          parts = parts.drop(3)
          None
      } else {
        parts = parts.drop(3)
        None
      }
      var comment = parts.mkString(" ").trim
      if (comment.startsWith("- ")) {
        comment = comment.drop(2)
      } else if (comment.startsWith("[ : ] - ")) {
        comment = comment.drop(8)
      }

      if (comment.trim.isEmpty()) {
        //println(s"nocomment - $filename - $filesize - $songlength - ${authors.mkString(",")}")
        metas += ModsAnthologyMeta(
          md5.get,
          path,
          filesize,
          songlength,
          authors.sorted,
          publishers = Buffer.empty,
          album = "",
          year = None
        )
        break()
      }
      var nocoopPattern = ""
      val nocoopPatterns = authorPatterns

      var authors_ = Buffer() ++ authors
      if (authors_.size > 1 ) boundary {
        for (pattern <- nocoopPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            authors_ = Buffer.empty
            var nocoop = m.group(1).trim
            if (nocoop == "Mush") nocoop = "Mushies"
            if (nocoop.contains(" & ")) authors_ ++= nocoop.split(" & ").map(_.trim)
            else authors_ += nocoop.trim
            authors_ = authors_.map(_
              .replaceAll("[ ]?/.*", "")
              .replaceAll("!$", ""))
            nocoopPattern = pattern.regex
            break()
          }
        }
      }
      authors_ = authors_.distinct

      var coopPattern = ""
      boundary {
        for (pattern <- coopPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            var coop = m.group(1).trim
            if (coop.contains(" & ")) authors_ ++= coop.split(" & ").map(_.trim)
            else authors_ += coop.trim
            coopPattern = pattern.regex
            break()
          }
        }
      }
      authors_ = authors_.distinct

      var album = ""
      var publishers = Buffer[String]()

      var yearPattern = ""
      var year: Option[Int] = None

      if (!comment.matches(".*[Cc]onverted [Bb]y \\w+ [Oo]n .*")) boundary {
        for (pattern <- yearPatterns if year.isEmpty) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            try {
              val extractedYear = m.group(1).toInt
              if ((extractedYear > 80 && extractedYear < 97) || (extractedYear > 1980 && extractedYear < 1997)) {
                year = Some(if (extractedYear < 100) 1900 + extractedYear else extractedYear)
                yearPattern = pattern.regex
              }
            } catch {
              case _: Exception => //println(s"Failed to extract year from ${m.group(1)}")
            }
            if (year.isDefined) break()
          }
        }
      }

      var publisherPattern = ""
      val (publishers_, publisherPattern_) = parsePublishers(publishers, comment)
      publishers = publishers_
      publisherPattern = publisherPattern_

      year = parsePartyYear(year, comment)

      val (album_, publishers__, publisherPattern__, year_) = parseParty(album, publishers, publisherPattern, year, comment)
      album = album_
      publishers = publishers__
      publisherPattern = publisherPattern__
      year = year_

      val (album__, albumPattern, publishers___) = parseAlbum(album, publishers, comment)
      album = album__
      publishers = publishers___
 
      // quirks

      publishers = normalizePublishers(publishers, authors, year)

      val (authors___, album___, publishers____, year___) = applyQuirks(authors_, album, publishers, year, comment)
      authors_ = authors___
      album = album___
      publishers = publishers____
      year = year___

      //println(s"$filename - $filesize - $songlength - ${authors_.mkString(",")} - ${album} - ${publishers.mkString(",")} - ${year.getOrElse(0)} --- $comment --- p:${publisherPattern} y:${yearPattern} a:${albumPattern} c:${coopPattern} nc:${nocoopPattern}")
      metas += ModsAnthologyMeta(
        md5.get,
        path,
        filesize,
        songlength,
        authors_.sorted,
        publishers.sorted,
        album,
        year
      )
    } else {
      //println(s"Skipping: $line")
    }
  }
  metas
).get

lazy val maz4txt = Paths.get(modsanthology_path + "Mods-1/Lists/Ascii/MAZ4-Groups.txt").toFile

def parseMazGroupsTxt(f: java.io.File) = Using(scala.io.Source.fromFile(f)(scala.io.Codec.ISO8859))(s =>
  val lines = s.getLines
  boundary {
    for (line <- lines) if (line.startsWith("==> Directory of Mods-")) break()
  }
  lines.next()
  val groupMap = mutable.Map[String, String]()
  boundary {
    for (line <- lines) {
      if (line.trim.isEmpty) break()
      // Abyss        = Abyss                   <121>    9145k - (Great Chip-Tunes by Pink... other tunes by Mem o'Ree & Neurodancer)
      val Array(abbr, name) = line.split("=")
      groupMap += abbr.trim.toLowerCase -> name.split("<")(0).trim.replace("_", " ").replace("-", " ")
    }
  }
  boundary {
    for (line <- lines) if (line == SEPARATOR) break()
  }
  val metas = Buffer[ModsAnthologyMeta]()
  var groupShort = ""
  var group = ""
  while (lines.hasNext) boundary {
    val line = lines.next().trim
    if (line.startsWith("MAZ")) {
      groupShort = line.split(":")(1)
      group = groupMap(groupShort.toLowerCase)
    } else if (line.matches(".*\\..*\\s+[0-9]+.*")) {
      var parts = line.split("\\s+")
      if (parts.length < 2) break()
      val filename = parts(0).trim
      val filesize = parts(1).trim.toInt
      val path = groupShort + "/" + filename
      val md5 = modsanthology_by_path.get(path).headOption.map(_.head.md5)
      if (md5.isEmpty) {
        System.err.println(s"WARN: modsanthology missing md5 for ${path}")
        break()
      }
      val songlength = if (parts.length > 2 && parts(2).startsWith("-")) try {
        val Array(min, sec) = parts(3).drop(1).dropRight(1).split(":")
        parts = parts.drop(5)
        Some(min.toInt * 60 + sec.toInt)
      } catch {
        case _: Exception =>
          parts = parts.drop(3)
          None
      } else {
        parts = parts.drop(3)
        None
      }
      var comment = parts.mkString(" ").trim
      if (comment.startsWith("- ")) {
        comment = comment.drop(2)
      } else if (comment.startsWith("[ : ] - ")) {
        comment = comment.drop(8)
      }

      var authors = Buffer.empty[String]
      var album = ""
      var publishers = Buffer(group)
      var year: Option[Int] = None

      if (group == "Triangle") {
        album = "Gigademo II"
        year = Some(1989)
      }
      if (group == "Wild Copper" && filename.startsWith("Megalo-")) {
        album = "Wild Copper's Megalo Demos"
        year = Some(1988)
      }
      if (group == "Tragedy" && filename.startsWith("Fatal_Morgana_")) {
        album = "Fatal Morgana"
        year = Some(1993)
      }
      if (group == "The Link") {
        album = "Megademo"
        authors += "Hurricane"
        year = Some(1989)
      }
      if (group == "The Giants") {
        album = "Megademo"
        year = Some(1990)
      }
      if (group == "Solution") {
        album = "Odeon"
        year = Some(1992)
      }
      if (group == "Silents" && filename.startsWith("B-House_")) {
        album = "Blues House"
        authors = Buffer("Blaizer", "Marillion")
        year = Some(1991)
      }
      if (group == "Scoopex" && filename.startsWith("MD-")) {
        publishers = Buffer("Scoopex", "Share and Enjoy")
        album = "Megademo"
        year = Some(1989)
      }
      if (group == "Rebels" && filename.startsWith("MD1-")) {
        album = "Megademo 1"
        year = Some(1989)
      }
      if (group == "Rebels" && filename.startsWith("MD2-")) {
        album = "Megademo 2"
        year = Some(1990)
      }
      if (group == "RamJam" && filename.startsWith("STYLE_")) {
        album = "Just a Matter of Style"
        year = Some(1994)
      }
      if (group == "RamJam" && filename.startsWith("TD_")) {
        album = "New Age"
        year = Some(1993)
      }
      if (group == "Predators") {
        album = "Megademo"
        year = Some(1989)
      }
      if (group == "Phenomena" && filename.startsWith("MDream1-")) {
        album = "Music Dream I"
        year = Some(1991)
      }
      if (group == "Phenomena" && filename.startsWith("MegaDemo-")) {
        album = "Megademo"
        year = Some(1989)
      }
      if (group == "Noxious" && filename.startsWith("Nox-")) {
        album = "Chip'n'Dip"
        year = Some(1993)
      }
      if (group == "Nova" && filename.startsWith("TD-")) {
        album = "Essential Classics"
        year = Some(1993)
      }
      if (group == "LSD" && filename.startsWith("Joes-")) {
        album = "Jesus on E's"
        year = Some(1992)
      }
      if (group == "Kefrens" && filename.startsWith("Desert_Dream-")) {
        album = "Desert Dream"
        year = Some(1993)
      }
      if (group == "Kefrens" && filename.startsWith("MD1-")) {
        album = "Megademo 5"
        year = Some(1989)
      }
      if (group == "Kefrens" && filename.startsWith("MD7-")) {
        album = "Megademo 7"
        year = Some(1989)
      }
      if (group == "Kefrens" && filename.startsWith("MD8-")) {
        album = "Megademo VIII"
        year = Some(1990)
      }
      if (group == "IT" && filename.startsWith("MD-")) {
        album = "Megademo"
        year = Some(1989)
      }
      if (group == "Force Ten" && filename.matches("^[0-9]_.*")) {
        album = "Force Ten Music Pack #" + filename(0)
        year = if (filename(0).toInt <= 4) Some(1994) else Some(1995)
      }
      if (group == "Fashion") {
        album = "Musicdisk"
        year = Some(1988)
      }
      if (group == "Essence" && filename.startsWith("Brainfood-")) {
        authors += "Dascon"
        album = "Brainfood " + filename.drop("Brainfood-".length).take(2)
      }
      if (group == "Essence" && filename.startsWith("Enchantment-")) {
        authors += "Covert Action Team"
      }
      if (group == "Essence" && filename.startsWith("Rom-")) {
        album = "ROM " + filename.drop("Rom-".length).take(1)
      }
      if (group == "Dexion" && filename.startsWith("MD1-")) {
        album = "Megademo"
        year = Some(1989)
      }
      if (group == "Dexion" && filename.startsWith("MD2-")) {
        album = "Megademo 2"
        year = Some(1989)
      }
      if (group == "DeathStar" && filename.startsWith("MD-")) {
        album = "Mega Demo"
        year = Some(1989)
      }
      if (group == "DAI" && filename.startsWith("TD-")) {
        album = "Musicdisk 4"
        year = Some(1993)
      }
      if (group == "Cryptoburners" && filename.startsWith("MD-")) {
        album = "Megademo"
        year = Some(1989)
      }
      if (group == "Cryptoburners" && filename.startsWith("MD2-")) {
        album = "Megademo II"
        year = Some(1990)
      }
      if (group == "Brainstormers" && filename.startsWith("TD-")) {
        publishers = Buffer("Brainstormers","Raid")
        album = "Mega Music Disk #1"
        year = Some(1989)
      }
      if (group == "Brainstorm" && filename.startsWith("MD-")) {
        album = "Persistence Megademo"
        year = Some(1990)
      }
      if (group == "Beastie Boys" && filename.startsWith("MD-")) {
        album = "Megademo"
        year = Some(1988)
      }
      if (group == "Alcatraz" && filename.startsWith("Ilyad-")) {
        publishers = Buffer("Alcatraz","Asphyxia Design")
        album = "Ilyad"
        year = Some(1994)
      }
      if (group == "Alcatraz" && filename.startsWith("MD2-")) {
        album = "Megademo"
        year = Some(1988)
      }
      if (group == "Alcatraz" && filename.startsWith("MD3-")) {
        album = "Megademo 3"
        year = Some(1989)
      }
      if (group == "Alcatraz" && filename.startsWith("MD4-")) {
        album = "Megademo IV: Devil's Key"
        year = Some(1990)
      }
      if (group == "Abyss" && filename.startsWith("DiSiSSid_")) {
        album = "Disissid " + filename.drop("DiSiSSid_".length).take(1)
        year = Some(1994)
      }
      if (group == "Abyss" && filename.startsWith("DizneeL")) {
        album = "Diznee Land " + filename.drop("DizneeL".length).take(1)
        year = Some(1994)
      }
      if (group == "Abyss" && filename.startsWith("Orange8_")) {
        album = "Orange 8"
        year = Some(1995)
      }
      if (group == "Radar Contrast Prods") {
        authors = Buffer("Contrast", "Radar")
        publishers = Buffer("Radar^Contrast^Productions")
      }
      if (comment.trim.isEmpty()) {
        //println(s"nocomment - $filename - $filesize - $songlength - ${authors.mkString(",")} - ${album} - ${publishers.mkString(",")} - ${year.getOrElse(0)}")
        metas += ModsAnthologyMeta(
          md5.get,
          path,
          filesize,
          songlength,
          authors.sorted,
          publishers = normalizePublishers(publishers, authors, None).sorted,
          album,
          year,
        )
        break()
      }

      var authorPattern = ""
      boundary {
        for (pattern <- authorPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            var author = m.group(1).trim
            if (author.matches(".*,.* and .*")) authors = author.split(" and |, ").map(_.trim).sorted.toBuffer
            else if (author.contains(" & ") && author.length > 5) authors = author.split(" & ").map(_.trim).sorted.toBuffer
            else if (!author.contains(" & ") && author.contains("&") && author.length > 3) authors = author.split("&").map(_.trim).sorted.toBuffer
            else if (author.contains("+") && author.length > 3) authors = author.split("\\+").map(_.trim).sorted.toBuffer
            else if (author.contains("^") && author.length > 3) authors = author.split("\\^").map(_.trim).sorted.toBuffer
            else if (author.contains(" and ")) authors = author.split(" and ").map(_.trim).sorted.toBuffer
            else authors = Buffer(author.trim)
            authors = authors.map(_
              .replaceAll("[ ]?/.*", "")
              .replaceAll("!$", ""))
            authorPattern = pattern.regex
            break()
          }
        }
      }
      authors = authors.distinct

      var yearPattern = ""
      if (!comment.matches(".*[Cc]onverted [Bb]y \\w+ [Oo]n .*")) boundary {
        for (pattern <- yearPatterns if year.isEmpty) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            try {
              val extractedYear = m.group(1).toInt
              if ((extractedYear > 80 && extractedYear < 97) || (extractedYear > 1980 && extractedYear < 1997)) {
                year = Some(if (extractedYear < 100) 1900 + extractedYear else extractedYear)
                yearPattern = pattern.regex
              }
            } catch {
              case _: Exception => //println(s"Failed to extract year from ${m.group(1)}")
            }
            if (year.isDefined) break()
          }
        }
      }
 
      var publisherPattern = ""
      year = parsePartyYear(year, comment)

      val (album_, albumPattern, _) = parseAlbum(album, Buffer.empty, comment)
      album = album_
  
      if (album.isEmpty) {
        val (album__, publishers_, publisherPattern_, year_) = parseParty(album, Buffer.empty, publisherPattern, year, comment)
        album = album__.trim
        publishers = if (publishers_.isEmpty) publishers else publishers_
        publisherPattern = publisherPattern_
        year = year_
      }

      publishers = normalizePublishers(publishers, authors, year)

      val (authors__, album__, publishers__, year__) = applyQuirks(authors, album, publishers, year, comment)
      authors = authors__
      album = album__
      publishers = publishers__
      year = year__

      //println(s"$filename - $filesize - $songlength - ${authors.mkString(",")} - ${album} - ${publishers.mkString(",")} - ${year.getOrElse(0)} --- $comment --- p:${publisherPattern} y:${yearPattern} a:${albumPattern} nc:${authorPattern}")

      metas += ModsAnthologyMeta(
        md5.get,
        path,
        filesize,
        songlength,
        authors.sorted,
        publishers.sorted,
        album,
        year
      )
    } else {
      //println(s"Skipping: $line")
    }
  }
  metas
).get

lazy val maz5txt = Paths.get(modsanthology_path + "Mods-1/Lists/Ascii/MAZ5-Misc.txt").toFile
def parseMazMiscTxt(f: java.io.File) = Using(scala.io.Source.fromFile(f)(scala.io.Codec.ISO8859))(s =>
  val lines = s.getLines
  boundary {
    for (line <- lines) if (line.startsWith("==> Directory of Mods-")) break()
  }
  lines.next()
  boundary {
    for (line <- lines) {
      if (line.trim.isEmpty) break()
    }
  }
  boundary {
    for (line <- lines) if (line == SEPARATOR) break()
  }
  val metas = Buffer[ModsAnthologyMeta]()
  var pathPrefix = ""
  var stickyPublishers = Buffer.empty[String]
  var stickyYear: Option[Int] = None
  while (lines.hasNext) boundary {
    val line = lines.next().trim

    if (line.startsWith("MAZ5:Compos")) {
      pathPrefix = line.split(":")(1).split("/").drop(1).mkString("/")
      line match {
        case "MAZ5:Compos/20mc" => stickyPublishers = Buffer("20 Minute ChipTune Compo")
        case "MAZ5:Compos/Asm94.4CH" =>
          stickyPublishers = Buffer("Assembly")
          stickyYear = Some(1994)
        case "MAZ5:Compos/Asm94.MCH" =>
          stickyPublishers = Buffer("Assembly")
          stickyYear = Some(1994) 
        case "MAZ5:Compos/Asm95" =>
          stickyPublishers = Buffer("Assembly")
          stickyYear = Some(1995)
        case "MAZ5:Compos/Juhla_95.2" =>
          stickyPublishers = Buffer("Juhla")
          stickyYear = Some(1995)
        case "MAZ5:Compos/SouthSea.94" =>
          stickyPublishers = Buffer("South Sealand")
          stickyYear = Some(1994)
        case "MAZ5:Compos/TG94" =>
          stickyPublishers = Buffer("The Gathering")
          stickyYear = Some(1994)
        case "MAZ5:Compos/TG95" =>
          stickyPublishers = Buffer("The Gathering")
          stickyYear = Some(1995)
        case "MAZ5:Compos/TG96" =>
          stickyPublishers = Buffer("The Gathering")
          stickyYear = Some(1996)
        case "MAZ5:Compos/TP92" =>
          stickyPublishers = Buffer("The Party")
          stickyYear = Some(1992)
        case "MAZ5:Compos/TP93" =>
          stickyPublishers = Buffer("The Party")
          stickyYear = Some(1993)
        case "MAZ5:Compos/TP94.4CH" =>
          stickyPublishers = Buffer("The Party")
          stickyYear = Some(1994)
        case "MAZ5:Compos/TP94.MCH" =>
          stickyPublishers = Buffer("The Party")
          stickyYear = Some(1994) 
        case "MAZ5:Compos/TP95.4CH" =>
          stickyPublishers = Buffer("The Party")
          stickyYear = Some(1995)
        case "MAZ5:Compos/TP95.MCH" =>
          stickyPublishers = Buffer("The Party")
          stickyYear = Some(1995)
        case "MAZ5:Compos/TT" =>
          stickyPublishers = Buffer.empty
          stickyYear = None
        case "MAZ5:Compos/Wired_95" =>
          stickyPublishers = Buffer("Wired")
          stickyYear = Some(1995)
        case _ =>
          stickyPublishers = Buffer.empty
          stickyYear = None
      }

    } else if (line.startsWith("MAZ")) {
      pathPrefix = line.split(":")(1)
      stickyPublishers = Buffer.empty
      stickyYear = None

    } else if (line.matches(".*\\..*\\s+[0-9]+.*")) {
      var parts = line.split("\\s+")
      if (parts.length < 2) break()
      val filename = parts(0).trim
      val filesize = try {
        parts(1).trim.toInt
      } catch {
        case _: Exception => break()
      }
      val path = pathPrefix + "/" + filename
      val md5 = modsanthology_by_path.get(path).headOption.map(_.head.md5)
      if (md5.isEmpty) {
        System.err.println(s"WARN: modsanthology missing md5 for ${path}")
        break()
      }
      val songlength = if (parts.length > 2 && parts(2).startsWith("-")) try {
        val Array(min, sec) = parts(3).drop(1).dropRight(1).split(":")
        parts = parts.drop(5)
        Some(min.toInt * 60 + sec.toInt)
      } catch {
        case _: Exception =>
          parts = parts.drop(3)
          None
      } else {
        parts = parts.drop(3)
        None
      }
      var comment = parts.mkString(" ").trim
      if (comment.startsWith("- ")) {
        comment = comment.drop(2)
      } else if (comment.startsWith("[ : ] - ")) {
        comment = comment.drop(8)
      }

      var authors = Buffer.empty[String]
      var album = ""
      var publishers = stickyPublishers
      var year = stickyYear

      if (comment.trim.isEmpty()) {
        if (!publishers.isEmpty && year.isEmpty) {
          //println(s"nocomment - $filename - $filesize - $songlength - ${authors.mkString(",")} - ${album} - ${publishers.mkString(",")} - ${year.getOrElse(0)}")
          metas += ModsAnthologyMeta(
            md5.get,
            path,
            filesize,
            songlength,
            authors.sorted,
            publishers = normalizePublishers(publishers, authors, None).sorted,
            album,
            year,
          )
        }
        break()
      }

      var authorPattern = ""
      boundary {
        for (pattern <- authorPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            var author = m.group(1).trim
            if (author.matches(".*,.* and .*")) authors = author.split(" and |, ").map(_.trim).sorted.toBuffer
            else if (author.contains(" & ") && author.length > 5) authors = author.split(" & ").map(_.trim).sorted.toBuffer
            else if (!author.contains(" & ") && author.contains("&") && author.length > 3) authors = author.split("&").map(_.trim).sorted.toBuffer
            else if (author.contains("+") && author.length > 3) authors = author.split("\\+").map(_.trim).sorted.toBuffer
            else if (author.contains("^") && author.length > 3) authors = author.split("\\^").map(_.trim).sorted.toBuffer
            else if (author.contains(" and ")) authors = author.split(" and ").map(_.trim).sorted.toBuffer
            else authors = Buffer(author.trim)
            authors = authors.map(_
              .replaceAll("[ ]?/.*", "")
              .replaceAll("!$", ""))
            authorPattern = pattern.regex
            break()
          }
        }
      }
      authors = authors.distinct

      var coopPattern = ""
      boundary {
        for (pattern <- coopPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            var coop = m.group(1).trim
            if (coop.contains(" & ")) authors ++= coop.split(" & ").map(_.trim)
            else authors += coop.trim
            coopPattern = pattern.regex
            break()
          }
        }
      }
      authors = authors.distinct

      var yearPattern = ""
      if (!comment.matches(".*[Cc]onverted [Bb]y \\w+ [Oo]n .*")) boundary {
        for (pattern <- yearPatterns if year.isEmpty) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            try {
              val extractedYear = m.group(1).toInt
              if ((extractedYear > 80 && extractedYear < 97) || (extractedYear > 1980 && extractedYear < 1997)) {
                year = Some(if (extractedYear < 100) 1900 + extractedYear else extractedYear)
                yearPattern = pattern.regex
              }
            } catch {
              case _: Exception => //println(s"Failed to extract year from ${m.group(1)}")
            }
            if (year.isDefined) break()
          }
        }
      }
 
      var publisherPattern = ""
      val (publishers_, publisherPattern_) = parsePublishers(publishers, comment)
      publishers = publishers_
      publisherPattern = publisherPattern_

      year = parsePartyYear(year, comment)

      val (album_, albumPattern, _) = parseAlbum(album, Buffer.empty, comment)
      album = album_
  
      if (album.isEmpty) {
        val (album__, publishers_, publisherPattern_, year_) = parseParty(album, Buffer.empty, publisherPattern, year, comment)
        album = album__.trim
        publishers = if (publishers_.isEmpty) publishers else publishers_
        publisherPattern = publisherPattern_
        year = year_
      }

      publishers = normalizePublishers(publishers, authors, year)

      val (authors__, album__, publishers__, year__) = applyQuirks(authors, album, publishers, year, comment)
      authors = authors__
      album = album__
      publishers = publishers__
      year = year__

      //println(s"$filename - $filesize - $songlength - ${authors.mkString(",")} - ${album} - ${publishers.mkString(",")} - ${year.getOrElse(0)} --- $comment --- p:${publisherPattern} y:${yearPattern} a:${albumPattern} c:${coopPattern} nc:${authorPattern}")

      metas += ModsAnthologyMeta(
        md5.get,
        path,
        filesize,
        songlength,
        authors.sorted,
        publishers.sorted,
        album,
        year
      )
    } else {
      //println(s"Skipping: $line")
    }
  }
  metas
).get

lazy val maz6txt = Paths.get(modsanthology_path + "Mods-1/Lists/Ascii/MAZ6-Synth.txt").toFile
def parseMazSynthTxt(f: java.io.File) = Using(scala.io.Source.fromFile(f)(scala.io.Codec.ISO8859))(s =>
  val lines = s.getLines
  boundary {
    for (line <- lines) if (line.startsWith("==> Directory of Mods-")) break()
  }
  lines.next()
  boundary {
    for (line <- lines) {
      if (line.trim.isEmpty) break()
    }
  }
  boundary {
    for (line <- lines) if (line == SEPARATOR) break()
  }
  val metas = Buffer[ModsAnthologyMeta]()
  var pathPrefix = ""
  var skip = false
  while (lines.hasNext) boundary {
    val line = lines.next().trim
    if (line.startsWith("MAZ6:AY/") ||
        line.startsWith("MAZ6:PlaySID/")
    ) {
      skip = true
    } else if (line.startsWith("MAZ6:")) {
      pathPrefix = line.split(":")(1)
      skip = false
    } else if (!skip && line.matches(".*\\..*\\s+[0-9]+.*")) {
      var parts = line.split("\\s+")
      if (parts.length < 2) break()
      val filename = parts(0).trim
      val filesize = try {
        parts(1).trim.toInt
      } catch {
        case _: Exception => break()
      }
      val path = pathPrefix + "/" + filename
      val md5 = modsanthology_by_path.get(path).headOption.map(_.head.md5)
      if (md5.isEmpty) {
        System.err.println(s"WARN: modsanthology missing md5 for ${path}")
        break()
      }
      val songlength = if (parts.length > 2 && parts(2).startsWith("-")) try {
        val Array(min, sec) = parts(3).drop(1).dropRight(1).split(":")
        parts = parts.drop(5)
        Some(min.toInt * 60 + sec.toInt)
      } catch {
        case _: Exception =>
          parts = parts.drop(3)
          None
      } else {
        parts = parts.drop(3)
        None
      }
      var comment = parts.mkString(" ").trim
      if (comment.startsWith("- ")) {
        comment = comment.drop(2)
      } else if (comment.startsWith("[ : ] - ")) {
        comment = comment.drop(8)
      }

      if (comment.trim.isEmpty() || comment.toLowerCase.matches("^[0-9]+ subsongs$")) break()

      var authors = Buffer.empty[String]
      var album = ""
      var publishers = Buffer.empty[String]
      var year: Option[Int] = None

      var authorPattern = ""
      boundary {
        for (pattern <- authorPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            var author = m.group(1).trim
            if (author.matches(".*,.* and .*")) authors = author.split(" and |, ").map(_.trim).sorted.toBuffer
            else if (author.contains(" & ") && author.length > 5) authors = author.split(" & ").map(_.trim).sorted.toBuffer
            else if (!author.contains(" & ") && author.contains("&") && author.length > 3) authors = author.split("&").map(_.trim).sorted.toBuffer
            else if (author.contains("+") && author.length > 3) authors = author.split("\\+").map(_.trim).sorted.toBuffer
            else if (author.contains("^") && author.length > 3) authors = author.split("\\^").map(_.trim).sorted.toBuffer
            else if (author.contains(" and ")) authors = author.split(" and ").map(_.trim).sorted.toBuffer
            else authors = Buffer(author.trim)
            authors = authors.map(_
              .replaceAll("[ ]?/.*", "")
              .replaceAll("!$", ""))
            authorPattern = pattern.regex
            break()
          }
        }
      }
      authors = authors.distinct

      var coopPattern = ""
      boundary {
        for (pattern <- coopPatterns) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            var coop = m.group(1).trim
            if (coop.contains(" & ")) authors ++= coop.split(" & ").map(_.trim)
            else authors += coop.trim
            coopPattern = pattern.regex
            break()
          }
        }
      }
      authors = authors.distinct

      var yearPattern = ""
      if (!comment.matches(".*[Cc]onverted [Bb]y \\w+ [Oo]n .*")) boundary {
        for (pattern <- yearPatterns if year.isEmpty) {
          pattern.findFirstMatchIn(comment).foreach { m =>
            try {
              val extractedYear = m.group(1).toInt
              if ((extractedYear > 80 && extractedYear < 97) || (extractedYear > 1980 && extractedYear < 1997)) {
                year = Some(if (extractedYear < 100) 1900 + extractedYear else extractedYear)
                yearPattern = pattern.regex
              }
            } catch {
              case _: Exception => //println(s"Failed to extract year from ${m.group(1)}")
            }
            if (year.isDefined) break()
          }
        }
      }

      var publisherPattern = ""
      val (publishers_, publisherPattern_) = parsePublishers(publishers, comment)
      publishers = publishers_
      publisherPattern = publisherPattern_

      year = parsePartyYear(year, comment)

      val (album_, albumPattern, _) = parseAlbum(album, Buffer.empty, comment)
      album = album_
  
      if (album.isEmpty) {
        val (album__, publishers_, publisherPattern_, year_) = parseParty(album, Buffer.empty, publisherPattern, year, comment)
        album = album__.trim
        publishers = if (publishers_.isEmpty) publishers else publishers_
        publisherPattern = publisherPattern_
        year = year_
      }

      publishers = normalizePublishers(publishers, authors, year)

      val (authors__, album__, publishers__, year__) = applyQuirks(authors, album, publishers, year, comment)
      authors = authors__
      album = album__
      publishers = publishers__
      year = year__

      //println(s"$filename - $filesize - $songlength - ${authors.mkString(",")} - ${album} - ${publishers.mkString(",")} - ${year.getOrElse(0)} --- $comment --- p:${publisherPattern} y:${yearPattern} a:${albumPattern} c:${coopPattern} nc:${authorPattern}")

      if (authors.nonEmpty || publishers.nonEmpty || album.nonEmpty || year.isDefined) {
        metas += ModsAnthologyMeta(
          md5.get,
          path,
          filesize,
          songlength,
          authors.sorted,
          publishers.sorted,
          album,
          year
        )
      } else {
        //println(s"Skipping: $line")
      }
    } else {
      //println(s"Skipping: $line")
    }
  }
  metas
).get

lazy val metas = Seq(
  (maz1txt, parseMazAuthorsTxt),
  (maz2txt, parseMazAuthorsTxt),
  (maz3txt, parseMazAuthorsTxt),
  (maz4txt, parseMazGroupsTxt),
  (maz5txt, parseMazMiscTxt),
  (maz6txt, parseMazSynthTxt)
).par.flatMap(m => m._2(m._1)).seq.distinct
