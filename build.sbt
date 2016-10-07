name               := "MutagenTx"
version            := "0.3.0"
organization       := "de.sciss"
scalaVersion       := "2.11.8"
crossScalaVersions := Seq("2.11.8", "2.10.6")
description        := "An experiment with genetic algorithms"
homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))
resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"

fork in run := true

// ---- main dependencies ----

lazy val lucreVersion               = "3.3.1"
lazy val scalaColliderUGensVersion  = "1.16.0"
lazy val scalaColliderVersion       = "1.21.0"
lazy val scalaColliderSwingVersion  = "1.31.0"
lazy val soundProcessesVersion      = "3.8.0"
lazy val strugatzkiVersion          = "2.13.0"
lazy val fileUtilVersion            = "1.1.2"
lazy val prefuseVersion             = "1.0.1"
lazy val lucreSwingVersion          = "1.4.0"
lazy val processorVersion           = "0.4.0"
lazy val fileCacheVersion           = "0.3.3"
lazy val kollFlitzVersion           = "0.2.0"
lazy val subminVersion              = "0.2.1"
lazy val scissDSPVersion            = "1.2.2"
lazy val audioWidgetsVersion        = "1.10.0"

// ---- test dependencies ----

lazy val scalaTestVersion           = "3.0.0"
lazy val topologyVersion            = "1.0.0"

libraryDependencies ++= Seq(
  "de.sciss"      %% "lucre-confluent"          % lucreVersion,
  "de.sciss"      %% "lucre-bdb"                % lucreVersion,
  "de.sciss"      %% "fileutil"                 % fileUtilVersion,
  "de.sciss"      %  "prefuse-core"             % prefuseVersion,
  "de.sciss"      %% "lucreswing"               % lucreSwingVersion,
  "de.sciss"      %% "processor"                % processorVersion,
  "de.sciss"      %% "scalacolliderugens-core"  % scalaColliderUGensVersion,
  "de.sciss"      %  "scalacolliderugens-spec"  % scalaColliderUGensVersion,
  "de.sciss"      %% "strugatzki"               % strugatzkiVersion,
  "de.sciss"      %% "filecache-txn"            % fileCacheVersion,
  "de.sciss"      %% "soundprocesses-core"      % soundProcessesVersion,
  "de.sciss"      %% "kollflitz"                % kollFlitzVersion,
  "de.sciss"      %  "submin"                   % subminVersion,
  // MFCC and SOM
  "de.sciss"      %% "scissdsp"                 % scissDSPVersion,
  // "de.sciss"      %% "lucredata-views"          % lucreDataVersion,
  "de.sciss"      %% "scalacollider"            % scalaColliderVersion,
  "de.sciss"      %% "scalacolliderswing-core"  % scalaColliderSwingVersion,
  "de.sciss"      %% "scalacolliderswing-plotting" % scalaColliderSwingVersion,
  "de.sciss"      %% "audiowidgets-swing"       % audioWidgetsVersion,
  // Test
  "org.scalatest" %% "scalatest"                % scalaTestVersion % "test",
  "de.sciss"      %% "topology"                 % topologyVersion  % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint")

mainClass in assembly := Some("de.sciss.mutagentx.GeneratorApp")

assemblyMergeStrategy in assembly := {
  case PathList("org", "w3c", "dom", "events", xs @ _*) => MergeStrategy.first // bloody Apache Batik
  case x =>
    val old = (assemblyMergeStrategy in assembly).value
    old(x)
}
