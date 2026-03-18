// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2023-2025 Matti Tiainen <mvtiaine@cc.hut.fi>

//> using dep org.scala-lang.modules::scala-parallel-collections::1.2.0

import scala.collection.mutable.Buffer
import scala.collection.parallel.CollectionConverters._
import scala.util.Using

enum Source:
  case
    _1996Mods,
    AlteredPerception,
    AmbMod,
    AmigaMuseum,
    AmigaMegaDemos,
    AmigaMegaGames,
    AmigaScene,
    Aminet,
    AMP,
    ArtPacksAcidOrg,
    ArtScene,
    BlasterSoundBBS,
    CelicModules,
    ChiptuneCom,
    ClassicGameSoundtracks,
    Dacapo,
    DaJormas,
    Demodulate,
    DemozooLeftovers,
    DfxCollection,
    DrMusic,
    DrMusicLab,
    DynamicMusicStation,
    EuroScene1,
    EuroScene2,
    Flerp,
    FredTheGang2014,
    FreedomCD,
    FTPKloszArtPL,
    Fujiology,
    GroovyBytes,
    Haxorfi,
    Hornet,
    HotSoundVision,
    HotSoundVision2,
    ImphobiaDreams,
    ImphobiaDreams2,
    Kosmic,
    LemonAmiga,
    MassiveModCollection,
    MaximumMods,
    MazziveInjection,
    MBnet,
    MegaModMadness,
    Melcom,
    MidiMultimediaExchangeBBS,
    ModJP,
    ModArchive,
    Modiromppu,
    Modland,
    ModlandIncoming,
    ModPlanet,
    ModSoulBrother,
    ModsAnthology,
    ModulesInJP,
    ModulesPL,
    ModulyPL,
    Monstro,
    MrOherd,
    MultimediaSoundFactory,
    Music95,
    MusicBox1994,
    MusicBoxPC,
    MusicModSoundEffect,
    NetlabelArchive,
    NightshiftSoundVision,
    NoiseMusic,
    NostalgicPlayer,
    OldExotica,
    PlayItByYear,
    PowerModul1,
    PowerModul2,
    ProTrackerModulesGPack,
    RetroExo,
    RetroPlayWHDLoadPacks,
    Scene96,
    SceneOrg,
    SceneOrgLostFound,
    SceneSporg,
    SceneStorm,
    SceneXplorer,
    SceneXplorer2,
    SOAMC,
    SoundMod1,
    SoundMod2,
    SoundMusicMidiCollection2,
    SoundsTerrific,
    SoundsTerrificII,
    SoundwareCollection,
    TerraSoundLibrary,
    TheDarkCornerBBS,
    TheModuleCollection,
    TheSceneArchives,
    TheSoundLibrary,
    TheUltimateModCollection,
    TheUltimateMusicSound1,
    TheUltimateMusicSound2,
    TOSECMusic,
    TOSECMusicUnknown,
    Tundrah,
    UgamodCollection1,
    UgamodCollection2,
    Ultrasounds,
    UnExotica,
    WantedTeam,
    WeirdScienceMultimedia,
    WeirdScienceMultimedia2,
    WorldOfGameMods,
    WorldOfSound1,
    WorldOfSound2,
    Zakalwe,
    Unnamed

import Source._

import scala.deriving.*

final case class Constraint (
  path: String = "",
  _type: String = "",
  _platform: String = "",
  year: Int = 0
)
type C = Constraint
val C = Constraint

lazy val sourceConstraints: Map[Source, Seq[C]] = Map(
  _1996Mods -> Seq(C(year = 1997)),
  AlteredPerception -> Seq(C(year = 2000)),
  AmbMod -> Seq(C(year = 2000)),
  AmigaMuseum -> Seq(C(_type = "Game", _platform = "Amiga", year = 2004)),
  AmigaMegaDemos -> Seq(C(_type = "Demo", _platform = "Amiga")),
  AmigaMegaGames -> Seq(C(_type = "Game", _platform = "Amiga")),
  AmigaScene -> Seq(
    C(path = "Parties/"),
    C(_type = "Demo", _platform = "Amiga")
  ),
  Aminet -> Seq(
    C(path = "demo/funet/Euroscene"), // duplicates Euroscene* sources
    C(path = "game/", _type = "Game", _platform = "Amiga"),
    C(path = "demo/", _type = "Demo", _platform = "Amiga"),
    C(path = "mags/", _type = "Mag", _platform = "Amiga")
  ),
  // AMP
  ArtPacksAcidOrg -> Seq(C(year = 2004)),
  // ArtScene
  BlasterSoundBBS -> Seq(C(year = 1997)),
  CelicModules -> Seq(C(year = 1997)),
  // ChiptuneCom
  ClassicGameSoundtracks -> Seq(C(_platform = "Amiga")),
  Dacapo -> Seq(C(year = 1995)),
  DaJormas -> Seq(
    C(path = "productions/", _type = "Demo", _platform = "Amiga"),
    C(_platform = "Amiga")
  ),
  Demodulate -> Seq(C(_type = "Demo", year = 2005)),
  // DemozooLeftovers
  DfxCollection -> Seq(
    C(path = "AmigaDemoCD", _type = "Demo", _platform = "Amiga", year = 2000),
    C(year = 2000)
  ),
  DrMusic -> Seq(
    C(path = "DEMOS/", _type = "Demo", _platform = "PC", year = 1993),
    C(year = 1993)
  ),
  DrMusicLab -> Seq(C(year = 1994)),
  DynamicMusicStation -> Seq(C(year = 1995)),
  EuroScene1 -> Seq(
    C(path = "DEMOS/", _type = "Demo", _platform = "Amiga", year = 1994),
    C(path = "DISKMAGS/", _type = "Mag", _platform = "Amiga", year = 1994),
    C(year = 1994)
  ),
  EuroScene2 -> Seq(
    C(path = "DEMOS/", _type = "Demo", _platform = "Amiga", year = 1995),
    C(path = "DISKMAGS/", _type = "Mag", _platform = "Amiga", year = 1995),
    C(year = 1995)
  ),
  Flerp -> Seq(C(year = 2002)),
  FredTheGang2014 -> Seq(C(year = 2014)), // TODO unclear if 2014 is actually correct because timestamps are not usable
  FreedomCD -> Seq(
    C(path = "DEMOS/", _type = "Demo", _platform = "PC", year = 1995),
    C(year = 1995)
  ),
  FTPKloszArtPL -> Seq(C(year = 2005)),
  Fujiology -> Seq(
    C(path = "FALCON/", _type = "Demo", _platform = "Atari"),
    C(path = "JAGUAR/", _type = "Demo", _platform = "Atari"),
    C(path = "ST/", _type = "Demo", _platform = "Atari"),
    C(path = "TT/", _type = "Demo", _platform = "Atari"),
    C(path = "MAGS/", _type = "Mag", _platform = "Atari")
  ),
  GroovyBytes -> Seq(
    C(path = "DEMOS/", _type = "Demo", _platform = "PC", year = 1996),
    C(path = "DISKMAGS/", _type = "Mag", _platform = "PC", year = 1996),
    C(path = "MUSICDSK/", _type = "MusicDisk", _platform = "PC", year = 1996),
    C(year = 1996)
  ),
  // Haxorfi
  Hornet -> Seq(
    C(path = "demos/", _type = "Demo", _platform = "PC", year = 1998),
    C(path = "mags/", _type = "Mag", _platform = "PC", year = 1998),
    C(path = "party/", _type = "Demo", _platform = "PC", year = 1998),
    C(year = 1998)
  ),
  HotSoundVision -> Seq(
    C(path = "HOTSV/DEMO/", _type = "Demo", _platform = "PC", year = 1993),
    C(year = 1993))
  ,
  HotSoundVision2 -> Seq(
    C(path = "HOTSV2/DEMO/MUSIC/", _type = "MusicDisk", year = 1994),
    C(path = "HOTSV2/DEMO/", _type = "Demo", _platform = "PC", year = 1994),
    C(year = 1994)
  ),
  ImphobiaDreams -> Seq(
    C(path = "CD/DEMOS/", _type = "Demo", _platform = "PC", year = 1996),
    C(path = "CD/DISKMAGS/", _type = "Mag", _platform = "PC", year = 1996),
    C(path = "CD/INTROS/", _type = "Intro", _platform = "PC", year = 1996),
    C(path = "CD/REPORTS/", _type = "Report", _platform = "PC", year = 1996),
    C(path = "CD/MUSIC/DISKS/", _type = "MusicDisk", _platform = "PC", year = 1996),
    C(year = 1996)
  ),
  ImphobiaDreams2 -> Seq(
    C(path = "1/CD1/DEMOS/", _type = "Demo", _platform = "PC", year = 1998),
    C(path = "1/CD1/DISKMAGS/", _type = "Mag", _platform = "PC", year = 1998),
    C(path = "1/CD1/INTROS/", _type = "Intro", _platform = "PC", year = 1998),
    C(path = "1/CD1/REPORTS/", _type = "Report", _platform = "PC", year = 1998),
    C(path = "2/CD2/MUSIC/DISKS/", _type = "MusicDisk", _platform = "PC", year = 1998),
    C(year = 1998)
  ),
  Kosmic -> Seq(
    C(path = "mdisks/", _type = "MusicDisk", _platform = "PC", year = 2000),
    C(year = 2000)
  ),
  LemonAmiga -> Seq(
    C(path = "1987/", _platform = "Amiga", year = 1988),
    C(path = "Cracktro/", _type = "Cracktro", _platform = "Amiga", year = 2020),
    C(path = "Demos/", _type = "Demo", _platform = "Amiga", year = 2020),
    C(path = "GAMES/", _type = "Game", _platform = "Amiga", year = 2020),
    C(path = "LSD-Grapevine --/", _type = "Mag", _platform = "Amiga", year = 1995),
    C(year = 2020)
  ),
  MassiveModCollection -> Seq(C(year = 1995)),
  MaximumMods -> Seq(C(year = 1996)),
  MazziveInjection -> Seq(C(year = 1998)),
  MBnet -> Seq(C(year = 2002)),
  MegaModMadness -> Seq(C(year = 1994)),
  Melcom -> Seq(C(year = 2005)),
  MidiMultimediaExchangeBBS -> Seq(C(year = 1993)),
  ModJP -> Seq(C(year = 2001)),
  ModArchive -> Seq(
    C(path = "modarchive_2007", year = 2007),
    C(path = "modarchive_2008", year = 2008),
    C(path = "modarchive_2009", year = 2009),
    C(path = "modarchive_2010", year = 2010),
    C(path = "modarchive_2011", year = 2011),
    C(path = "modarchive_2012", year = 2012),
    C(path = "modarchive_2013", year = 2013),
    C(path = "modarchive_2014", year = 2014),
    C(path = "modarchive_2015", year = 2015),
    C(path = "modarchive_2016", year = 2016),
    C(path = "modarchive_2017", year = 2017),
    C(path = "modarchive_2018", year = 2018),
    C(path = "modarchive_2019", year = 2019),
    C(path = "modarchive_2020", year = 2020),
    C(path = "modarchive_2021", year = 2021),
    C(path = "modarchive_2022", year = 2022),
    C(path = "modarchive_2023", year = 2023),
    C(year = 2023)
  ),
  Modiromppu -> Seq(
    C(path = "DEMOS/", _type = "Demo", _platform = "PC", year = 1998),
    C(path = "MUSICDSK", _type = "MusicDisk", _platform = "PC", year = 1998),
    C(year = 1998)
  ),
  // Modland,
  // ModlandIncoming
  ModPlanet -> Seq(
    C(path = "cd2/demodulate/amiga demos/", _type = "Demo", _platform = "Amiga", year = 2002),
    C(path = "cd2/demodulate/amiga diskmags/", _type = "Mag", _platform = "Amiga", year = 2002),
    C(path = "cd2/demodulate/amiga intros/", _type = "Intro", _platform = "Amiga", year = 2002),
    C(path = "cd2/demodulate/pc demos/", _type = "Demo", _platform = "PC", year = 2002),
    C(path = "cd2/demodulate/pc diskmags/", _type = "Mag", _platform = "PC", year = 2002),
    C(path = "cd2/demodulate/pc intros/", _type = "Intro", _platform = "PC", year = 2002),
    C(path = "cd2/music disks/amiga/", _type = "MusicDisk", _platform = "Amiga", year = 2002),
    C(path = "cd2/music disks/pc/", _type = "MusicDisk", _platform = "PC", year = 2002),
    C(path = "bonuscd/demodulate/amiga intros/", _type = "Intro", _platform = "Amiga", year = 2006),
    C(path = "bonuscd/demodulate/pc demos/", _type = "Demo", _platform = "PC", year = 2006),
    C(path = "bonuscd/demodulate/pc intros/", _type = "Intro", _platform = "PC", year = 2006),
    C(path = "cd1/", year = 2002),
    C(path = "cd2/", year = 2002),
    C(path = "bonuscd/", year = 2006),
    C(year = 2006)
  ),
  ModSoulBrother -> Seq(C(year = 2005)),
  ModsAnthology -> Seq(C(year = 1996)),
  ModulesInJP -> Seq(C(year = 2003)),
  // ModulesPL
  ModulyPL -> Seq(C(year = 2007)),
  Monstro -> Seq(C(year = 2002)),
  MrOherd -> Seq(C(year = 2020)),
  MultimediaSoundFactory -> Seq(C(year = 1995)),
  Music95 -> Seq(C(year = 1997)),
  MusicBox1994 -> Seq(C(year = 1993)),
  MusicBoxPC -> Seq(C(year = 1995)),
  MusicModSoundEffect -> Seq(C(year = 1995)),
  // NetlabelArchive
  NightshiftSoundVision -> Seq(C(year = 1995)),
  NoiseMusic -> Seq(
    C(path = "1995/", year = 1995),
    C(path = "1996/", year = 1996),
    C(path = "1997/", year = 1997),
    C(path = "1998/", year = 1998),
    C(path = "1999/", year = 1999),
    C(path = "2000", year = 2000),
    C(year = 2002)
  ),
  // NostalgicPlayer
  OldExotica -> Seq(C(_platform = "Amiga", year = 2007)),
  PlayItByYear -> Seq(
    C(path = "1985", _type = "Game", _platform = "Amiga", year = 1985),
    C(path = "1986", _type = "Game", _platform = "Amiga", year = 1986),
    C(path = "1987", _type = "Game", _platform = "Amiga", year = 1987),
    C(path = "1988", _type = "Game", _platform = "Amiga", year = 1988),
    C(path = "1989", _type = "Game", _platform = "Amiga", year = 1989),
    C(path = "1990", _type = "Game", _platform = "Amiga", year = 1990),
    C(path = "1991", _type = "Game", _platform = "Amiga", year = 1991),
    C(path = "1992", _type = "Game", _platform = "Amiga", year = 1992),
    C(path = "1993", _type = "Game", _platform = "Amiga", year = 1993),
    C(path = "1994", _type = "Game", _platform = "Amiga", year = 1994),
    C(path = "1995", _type = "Game", _platform = "Amiga", year = 1995),
    C(path = "85-91.zip/85-91.hdf/1985", _type = "Game", _platform = "Amiga", year = 1985),
    C(path = "85-91.zip/85-91.hdf/1986", _type = "Game", _platform = "Amiga", year = 1986),
    C(path = "85-91.zip/85-91.hdf/1987", _type = "Game", _platform = "Amiga", year = 1987),
    C(path = "85-91.zip/85-91.hdf/1988", _type = "Game", _platform = "Amiga", year = 1988),
    C(path = "85-91.zip/85-91.hdf/1989", _type = "Game", _platform = "Amiga", year = 1989),
    C(path = "85-91.zip/85-91.hdf/1990", _type = "Game", _platform = "Amiga", year = 1990),
    C(path = "85-91.zip/85-91.hdf/1991", _type = "Game", _platform = "Amiga", year = 1991),
    C(path = "92-95.zip/92-95.hdf/1992", _type = "Game", _platform = "Amiga", year = 1992),
    C(path = "92-95.zip/92-95.hdf/1993", _type = "Game", _platform = "Amiga", year = 1993),
    C(path = "92-95.zip/92-95.hdf/1994", _type = "Game", _platform = "Amiga", year = 1994),
    C(path = "92-95.zip/92-95.hdf/1995", _type = "Game", _platform = "Amiga", year = 1995),
    C(_type = "Game", _platform = "Amiga")
  ),
  PowerModul1 -> Seq(C(year = 1995)),
  PowerModul2 -> Seq(C(year = 1995)),
  // ProTrackerModulesGPack
  RetroExo -> Seq(
    C(path = "exodemoscene/", _type = "Demo", _platform = "PC", year = 2023),
    C(path = "exowin9x/1994/", _type = "Game", _platform = "PC", year = 1994),
    C(path = "exowin9x/1995/", _type = "Game", _platform = "PC", year = 1995),
    C(path = "exowin9x/1996/", _type = "Game", _platform = "PC", year = 1996),
    C(path = "exowin3x/", _type = "Game", _platform = "PC", year = 2001),
    C(_platform = "PC")
  ),
  RetroPlayWHDLoadPacks -> Seq(
    C(path = "Commodore_Amiga_-_HD_Loaders_-_Games", _type = "Game", _platform = "Amiga"),
    C(path = "Commodore_Amiga_-_JST_-_Games", _type = "Game", _platform = "Amiga"),
    C(path = "Commodore_Amiga_-_WHDLoad_-_Demos", _type = "Demo", _platform = "Amiga"),
    C(path = "Commodore_Amiga_-_WHDLoad_-_Games", _type = "Game", _platform = "Amiga"),
    C(path = "Commodore_Amiga_-_WHDLoad_-_Magazines", _type = "Mag", _platform = "Amiga"),
    C(_platform = "Amiga")
  ),
  Scene96 -> Seq(
    C(path = "1/DEMOS/", _type = "Demo", _platform = "PC", year = 1997),
    C(path = "1/DISKMAGS/", _type = "Mag", _platform = "PC", year = 1997),
    C(path = "1/GAMES/", _type = "Game", _platform = "PC", year = 1997),
    C(path = "1/INTROS/", _type = "Intro", _platform = "PC", year = 1997),
    C(path = "1/PARTY/INVTROS/", _type = "Invitation", _platform = "PC", year = 1997),
    C(path = "1/PARTY/REPORTS/", _type = "Report", _platform = "PC", year = 1997),
    C(path = "2/MUSICDSK/", _type = "MusicDisk", _platform = "PC", year = 1997),
    C(year = 1997)
  ),
  SceneOrg -> Seq(
    C(path = "demos/", _type = "Demo"),
    C(path = "mags/", _type = "Mag"),
    C(path = "music/disks/", _type = "MusicDisk"),
    C(path = "parties/1987", year = 1987),
    C(path = "parties/1988", year = 1988),
    C(path = "parties/1989", year = 1989),
    C(path = "parties/1990", year = 1990),
    C(path = "parties/1991", year = 1991),
    C(path = "parties/1992", year = 1992),
    C(path = "parties/1993", year = 1993),
    C(path = "parties/1994/theparty94/misc/the_party_94_cdrom", year = 1995),
    C(path = "parties/1994", year = 1994),
    C(path = "parties/1995", year = 1995),
    C(path = "parties/1996", year = 1996),
    C(path = "parties/1997", year = 1997),
    C(path = "parties/1998", year = 1998),
    C(path = "parties/1999", year = 1999),
    C(path = "parties/2000", year = 2000),
    C(path = "parties/2001", year = 2001),
    C(path = "parties/2002", year = 2002),
    C(path = "parties/2003", year = 2003),
    C(path = "parties/2004", year = 2004),
    C(path = "parties/2005", year = 2005),
    C(path = "parties/2006", year = 2006),
    C(path = "parties/2007", year = 2007),
    C(path = "parties/2008", year = 2008),
    C(path = "parties/2009", year = 2009),
    C(path = "parties/2010", year = 2010),
    C(path = "parties/2011", year = 2011),
    C(path = "parties/2012", year = 2012),
    C(path = "parties/2013", year = 2013),
    C(path = "parties/2014", year = 2014),
    C(path = "parties/2015", year = 2015),
    C(path = "parties/2016", year = 2016),
    C(path = "parties/2017", year = 2017),
    C(path = "parties/2018", year = 2018),
    C(path = "parties/2019", year = 2019),
    C(path = "parties/2020", year = 2020),
    C(path = "parties/2021", year = 2021),
    C(path = "parties/2022", year = 2022),
    C(path = "parties/2023", year = 2023),
    C(path = "parties/2024", year = 2024),
    C(path = "parties/2025", year = 2025),
    C(path = "parties/2026", year = 2026)
  ),
  // SceneOrgLostFound
  SceneSporg -> Seq(
    C(path = "1994/", year = 1994),
    C(path = "1995/", year = 1995),
    C(path = "1997/", year = 1997),
    C(path = "2001/", year = 2001),
    C(path = "2002/", year = 2002),
    C(path = "2003/", year = 2003),
    C(path = "2004/", year = 2004),
    C(path = "2005/", year = 2005),
    C(path = "2006/", year = 2006),
    C(path = "2007/", year = 2007),
    C(path = "diskmags/", _type = "Mag", year = 2010),
    C(year = 2010)
  ),
  SceneStorm -> Seq(
    C(path = "Charts&Diskmags", _type = "Mag", _platform = "Amiga", year = 1997),
    C(path = "Demos/", _type = "Demo", _platform = "Amiga", year = 1997),
    C(path = "Intros/", _type = "Intro", _platform = "Amiga", year = 1997),
    C(path = "Music-Disks/", _type = "MusicDisk", _platform = "Amiga", year = 1997),
    C(path = "Slideshows/", _type = "SlideShow", _platform = "Amiga", year = 1997),
    C(year = 1997)
  ),
  SceneXplorer -> Seq(
    C(path = "!DEMOS!/", _type = "Demo", _platform = "Amiga", year = 1997),
    C(path = "!GRAPHICS!/Slideshows/", _type = "SlideShow", _platform = "Amiga", year = 1997),
    C(path = "!INTROS!/", _type = "Intro", _platform = "Amiga", year = 1997),
    C(path = "!MAGAZINES_&_PACKS/", _type = "Mag", _platform = "Amiga", year = 1997),
    C(path = "!MUSIC!/Music_disks/", _type = "MusicDisk", _platform = "Amiga", year = 1997),
    C(year = 1997)
  ),
  SceneXplorer2 -> Seq(
    C(path = "!DEMOS!/", _type = "Demo", _platform = "Amiga", year = 1998),
    C(path = "!GRAPHICS!/Slideshows/", _type = "SlideShow", _platform = "Amiga", year = 1998),
    C(path = "!INTROS!/", _type = "Intro", _platform = "Amiga", year = 1998),
    C(path = "!MAGS^PACKS!/", _type = "Mag", _platform = "Amiga", year = 1998),
    C(path = "!MUSIC!/Music_disks/", _type = "MusicDisk", _platform = "Amiga", year = 1998),
    C(year = 1998)
  ),
  SOAMC -> Seq(
    C(path = "000/AMIGA/Trackers_Games/", _type = "Game", _platform = "Amiga", year = 2021),
    C(year = 2021)
  ),
  SoundMod1 -> Seq(C(year = 1994)),
  SoundMod2 -> Seq(C(year = 1995)),
  SoundMusicMidiCollection2 -> Seq(C(year = 1996)),
  SoundsTerrific -> Seq(C(year = 1994)),
  SoundsTerrificII -> Seq(C(year = 1996)),
  SoundwareCollection -> Seq(C(year = 1993)),
  TerraSoundLibrary -> Seq(
    C(path = "DEMO/", _type = "Demo", _platform = "Amiga", year = 1995),
    C(year = 1995)
  ),
  TheDarkCornerBBS -> Seq(
    C(path = "Demo/", _type = "Demo", _platform = "PC", year = 2001),
    C(path = "Games/", _type = "Game", _platform = "PC", year = 2001),
    C(year = 2001)
  ),
  TheModuleCollection -> Seq(C(year = 1995)),
  TheSceneArchives -> Seq(C(_platform = "Amiga", year = 2002)),
  // TheSoundLibrary
  TheUltimateModCollection -> Seq(C(year = 1992)),
  TheUltimateMusicSound1 -> Seq(
    C(path = "MUS_DISK/", _type = "MusicDisk", _platform = "PC", year = 1995),
    C(year = 1995)
  ),
  TheUltimateMusicSound2 -> Seq(
    C(path = "MUS_DISK/", _type = "MusicDisk", _platform = "PC", year = 1996),
    C(year = 1996)
  ),
  TOSECMusic -> Seq(
    C(path = "Demos/", _type = "Demo", year = 2005),
    C(path = "Games/", _type = "Game", year = 2005),
    C(year = 2005)
  ),
  TOSECMusicUnknown -> Seq(
    C(path = "Music - Demos/", _type = "Demo", year = 2004),
    C(path = "Music - Games/", _type = "Game", year = 2004),
    C(year = 2004)
  ),
  Tundrah -> Seq(C(year = 2006)),
  UgamodCollection1 -> Seq(C(year = 1994)),
  UgamodCollection2 -> Seq(C(year = 1994)),
  Ultrasounds -> Seq(C(year = 1995)),
  UnExotica -> Seq(
    C(path = "Demo/", _type = "Demo", _platform = "Amiga", year = 2020),
    C(path = "Game/", _type = "Game", _platform = "Amiga", year = 2020),
    C(_platform = "Amiga", year = 2020)
  ),
  WantedTeam -> Seq(
    C(path="games/", _type = "Game", _platform = "Amiga", year = 2015),
    C(_platform = "Amiga", year = 2015)
  ),
  WeirdScienceMultimedia -> Seq(C(year = 1993)),
  WeirdScienceMultimedia2 -> Seq(C(year = 1995)),
  // WorldOfGameMods // has also Game remixes/covers etc.
  WorldOfSound1 -> Seq(C(year = 1994)),
  WorldOfSound2 -> Seq(C(year = 1996)),
  Zakalwe -> Seq(
    C(path = "benn_daglish_sid/"),
    C(path = "mod/"),
    C(path = "pokeynoise/"),
    C(path = "quartet_psg", _platform = "Atari"),
    C(path = "quartet_st", _platform = "Atari"),
    C(path = "special_fx_st", _type = "Game", _platform = "Atari"),
    C(path = "tfmx_st", _type = "Game", _platform = "Atari"),
    C(_platform = "Amiga")
  ),
  Unnamed -> Seq(
    C(path = "Amiga_Cracktros_Modules", _type = "Cracktro", _platform = "Amiga"),
    C(path = "Amiga_GameMusic", _platform = "Amiga"),  // has also non-game music
    C(path = "Amiga_Games_Modules", _type = "Game", _platform = "Amiga"),
    C(path = "Amiga.Music", _type = "Game", _platform = "Amiga"),
    C(path = "Other Amiga Music.7z/Games/", _type = "Game", _platform = "Amiga"),
    C(_platform = "Amiga")
  )
)

val tsvfiles = Buffer(
  ("bbs/blastersoundbbs.tsv", BlasterSoundBBS),
  ("bbs/mbnet.tsv", MBnet),
  ("bbs/midimultimediaexchangebbs.tsv", MidiMultimediaExchangeBBS),
  ("bbs/thedarkcornerbbs.tsv", TheDarkCornerBBS),
  ("cd/dacapo.tsv", Dacapo),
  ("cd/drmusic.tsv", DrMusic),
  ("cd/drmusiclab.tsv", DrMusicLab),
  ("cd/dynamicmusicstation.tsv", DynamicMusicStation),
  ("cd/euroscene1.tsv", EuroScene1),
  ("cd/euroscene2.tsv", EuroScene2),
  ("cd/freedomcd.tsv", FreedomCD),
  ("cd/groovybytes.tsv", GroovyBytes),
  ("cd/hotsoundvision.tsv", HotSoundVision),
  ("cd/hotsoundvision2.tsv", HotSoundVision2),
  ("cd/imphobiadreams.tsv", ImphobiaDreams),
  ("cd/imphobiadreams2.tsv", ImphobiaDreams2),
  ("cd/massivemodcollection.tsv", MassiveModCollection),
  ("cd/maximummods.tsv", MaximumMods),
  ("cd/megamodmadness.tsv", MegaModMadness),
  ("cd/modsanthology.tsv", ModsAnthology),
  ("cd/monstro.tsv", Monstro),
  ("cd/multimediasoundfactory.tsv", MultimediaSoundFactory),
  ("cd/music95.tsv", Music95),
  ("cd/musicbox1994.tsv", MusicBox1994),
  ("cd/musicboxpc.tsv", MusicBoxPC),
  ("cd/musicmodsoundeffect.tsv", MusicModSoundEffect),
  ("cd/nightshiftsoundvision.tsv", NightshiftSoundVision),
  ("cd/powermodul1.tsv", PowerModul1),
  ("cd/powermodul2.tsv", PowerModul2),
  ("cd/scene96.tsv", Scene96),
  ("cd/scenestorm.tsv", SceneStorm),
  ("cd/scenexplorer.tsv", SceneXplorer),
  ("cd/scenexplorer2.tsv", SceneXplorer2),
  ("cd/soundmod1.tsv", SoundMod1),
  ("cd/soundmod2.tsv", SoundMod2),
  ("cd/soundmusicmidicollection2.tsv", SoundMusicMidiCollection2),
  ("cd/soundsterrific.tsv", SoundsTerrific),
  ("cd/soundsterrificii.tsv", SoundsTerrificII),
  ("cd/soundwarecollection.tsv", SoundwareCollection),
  ("cd/terrasoundlibrary.tsv", TerraSoundLibrary),
  ("cd/themodulecollection.tsv", TheModuleCollection),
  ("cd/thesoundlibrary.tsv", TheSoundLibrary),
  ("cd/theultimatemodcollection.tsv", TheUltimateModCollection),
  ("cd/theultimatemusicsound1.tsv", TheUltimateMusicSound1),
  ("cd/theultimatemusicsound2.tsv", TheUltimateMusicSound2),
  ("cd/ugamodcollection1.tsv", UgamodCollection1),
  ("cd/ugamodcollection2.tsv", UgamodCollection2),
  ("cd/ultrasounds.tsv", Ultrasounds),
  ("cd/weirdsciencemultimedia.tsv", WeirdScienceMultimedia),
  ("cd/weirdsciencemultimedia2.tsv", WeirdScienceMultimedia2),
  ("cd/worldofsound1.tsv", WorldOfSound1),
  ("cd/worldofsound2.tsv", WorldOfSound2),
  ("collection/1996mods.tsv", _1996Mods),
  ("collection/alteredperception.tsv", AlteredPerception),
  ("collection/ambmod.tsv", AmbMod),
  ("collection/celicmodules.tsv", CelicModules),
  ("collection/dfxcollection.tsv", DfxCollection),
  ("collection/fredthegang2014.tsv", FredTheGang2014),
  ("collection/lemonamiga.tsv", LemonAmiga),
  ("collection/mazziveinjection.tsv", MazziveInjection),
  ("collection/melcom.tsv", Melcom),
  ("collection/mod_jp.tsv", ModJP),
  ("collection/modiromppu.tsv", Modiromppu),
  ("collection/modplanet.tsv", ModPlanet),
  ("collection/modulesinjp.tsv", ModulesInJP),
  ("collection/modulypl.tsv", ModulyPL),
  ("collection/mroherd.tsv", MrOherd),
  ("collection/playitbyyear.tsv", PlayItByYear),
  ("collection/protrackermodulesgpack.tsv", ProTrackerModulesGPack),
  ("collection/retroplaywhdloadpacks.tsv", RetroPlayWHDLoadPacks),
  ("collection/thescenearchives.tsv", TheSceneArchives),
  ("collection/tosecmusic.tsv", TOSECMusic),
  ("collection/tosecmusic_unknown.tsv", TOSECMusicUnknown),
  ("collection/tundrah.tsv", Tundrah),
  ("collection/unnamed.tsv", Unnamed),
  ("group/dajormas.tsv", DaJormas),
  ("group/kosmic.tsv", Kosmic),
  ("group/modsoulbrother.tsv", ModSoulBrother),
  ("group/netlabelarchive.tsv", NetlabelArchive),
  ("group/noisemusic.tsv", NoiseMusic),
  ("site/amigamega_demos.tsv", AmigaMegaDemos),
  ("site/amigamega_games.tsv", AmigaMegaGames),
  ("site/amigamuseum.tsv", AmigaMuseum),
  ("site/amigascne.tsv", AmigaScene),
  ("site/aminet.tsv", Aminet),
  ("site/amp.tsv", AMP),
  ("site/artpacksacidorg.tsv", ArtPacksAcidOrg),
  ("site/artscene.tsv", ArtScene),
  ("site/chiptunecom.tsv", ChiptuneCom),
  ("site/classicgamesoundtracks.tsv", ClassicGameSoundtracks),
  ("site/demodulate.tsv", Demodulate),
  ("site/demozoo_leftovers.tsv", DemozooLeftovers),
  ("site/flerp.tsv", Flerp),
  ("site/ftpkloszartpl.tsv", FTPKloszArtPL),
  ("site/fujiology.tsv", Fujiology),
  ("site/haxorfi.tsv", Haxorfi),
  ("site/hornet.tsv", Hornet),
  ("site/modarchive.tsv", ModArchive),
  ("site/modland.tsv", Modland),
  ("site/modland_incoming.tsv", ModlandIncoming),
  ("site/modulespl.tsv", ModulesPL),
  ("site/nostalgicplayer.tsv", NostalgicPlayer),
  ("site/oldexotica.tsv", OldExotica),
  ("site/retroexo.tsv", RetroExo),
  ("site/sceneorg.tsv", SceneOrg),
  ("site/sceneorg_lostfound.tsv", SceneOrgLostFound),
  ("site/scenesporg.tsv", SceneSporg),
  ("site/soamc.tsv", SOAMC),
  ("site/unexotica.tsv", UnExotica),
  ("site/wantedteam.tsv", WantedTeam),
  ("site/worldofgamemods.tsv", WorldOfGameMods),
  ("site/zakalwe.tsv", Zakalwe),
);

final case class TsvEntry (
  md5: String,
  subsong: Int,
  songlength: Int,
  songend: String,
  player: String,
  format: String,
  channels: Int,
  filesize: Int,
  xxh32: String,
  crc32: String,
  path: String,
)

lazy val tsvs = tsvfiles.par.map(tsv => (tsv._2, Using(scala.io.Source.fromFile(s"sources/${tsv._1}")(using scala.io.Codec.ISO8859))(tsv =>
  var player = ""
  tsv.getLines.map(line =>
    val l = line.split("\t")
    if (l.length > 4) {
      player = l(4)
      TsvEntry(l(0), l(1).toInt, l(2).toInt, l(3), player, l(5), if (l(6).isEmpty) 0 else l(6).toInt, l(7).toInt, l(8), l(9), l(10))
    } else TsvEntry(l(0), l(1).toInt, l(2).toInt, l(3), player, "", 0, -1, "", "", "")
  ).toBuffer
).get.sortBy(e => (e.md5, e.subsong)).groupBy(_.md5))).seq

final case class SourceDBEntry (
  md5: String,
  path: String,
  filesize: Int,
  xxh32: String,
  crc32: String
)

def readSourceDB(source: Source) = {
  tsvs.filter(_._1 == source).par.flatMap(_._2).map({case (md5,subsongs) =>
    if (subsongs.groupBy(_.subsong).exists(_._2.size > 1)) {
      System.err.println("INFO: duplicate files in " + source + " for " + md5 + ": " + subsongs)
    }
    subsongs.filter(_.path != "").map(e =>
      SourceDBEntry(md5, e.path, e.filesize, e.xxh32, e.crc32)
    )
  }).flatten.seq
}.toSeq

lazy val amigascne = readSourceDB(AmigaScene)
lazy val aminet = readSourceDB(Aminet)
lazy val amp = readSourceDB(AMP)
lazy val artpacksacidorg = readSourceDB(ArtPacksAcidOrg)
lazy val blastersoundbbs = readSourceDB(BlasterSoundBBS)
lazy val demodulate = readSourceDB(Demodulate)
lazy val demozoo_leftovers = readSourceDB(DemozooLeftovers)
lazy val flerp = readSourceDB(Flerp)
lazy val fujiology = readSourceDB(Fujiology)
lazy val hornet = readSourceDB(Hornet)
lazy val modland = readSourceDB(Modland)
lazy val modplanet = readSourceDB(ModPlanet)
lazy val modsanthology = readSourceDB(ModsAnthology)
lazy val modsoulbrother = readSourceDB(ModSoulBrother)
lazy val oldexotica = readSourceDB(OldExotica)
lazy val sceneorg = readSourceDB(SceneOrg)
lazy val sceneorg_lostfound = readSourceDB(SceneOrgLostFound)
lazy val scenesporg = readSourceDB(SceneSporg)
lazy val tosecmusic = readSourceDB(TOSECMusic)
lazy val tosecmusic_unknown = readSourceDB(TOSECMusicUnknown)
lazy val unexotica = readSourceDB(UnExotica)
lazy val wantedteam = readSourceDB(WantedTeam)
