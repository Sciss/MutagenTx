name               := "MutagenTx"

version            := "0.1.0"

organization       := "de.sciss"

scalaVersion       := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.10.5")

description        := "An experiment with genetic algorithms"

homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))

licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"

fork in run := true

// ---- main dependencies ----

lazy val lucreSTMVersion            = "2.1.2"
lazy val lucreDataVersion           = "2.3.2"
lazy val lucreEventVersion          = "2.7.4"
lazy val lucreConfluentVersion      = "2.11.1"
lazy val scalaColliderUGensVersion  = "1.13.1"
lazy val scalaColliderVersion       = "1.17.2"
lazy val scalaColliderSwingVersion  = "1.25.1"
lazy val soundProcessesVersion      = "2.18.1"
lazy val strugatzkiVersion          = "2.9.0"
lazy val fileUtilVersion            = "1.1.1"
lazy val prefuseVersion             = "1.0.0"
lazy val lucreSwingVersion          = "0.9.1"
lazy val processorVersion           = "0.4.0"
lazy val fileCacheVersion           = "0.3.3"
lazy val kollFlitzVersion           = "0.2.0"
lazy val webLaFVersion              = "1.28"
lazy val scissDSPVersion            = "1.2.2"

// ---- test dependencies ----

lazy val scalaTestVersion           = "2.2.5"
lazy val topologyVersion            = "1.0.0"

libraryDependencies ++= Seq(
  "de.sciss"      %% "lucreconfluent"           % lucreConfluentVersion,
  "de.sciss"      %% "lucreevent-expr"          % lucreEventVersion,
  "de.sciss"      %% "lucrestm-bdb"             % lucreSTMVersion,
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
  "de.sciss"      %  "weblaf"                   % webLaFVersion,
  // MFCC and SOM
  "de.sciss"      %% "scissdsp"                 % scissDSPVersion,
  "de.sciss"      %% "lucredata-core"           % lucreDataVersion,
  "de.sciss"      %% "lucredata-views"          % lucreDataVersion,
  "de.sciss"      %% "scalacollider"            % scalaColliderVersion,
  "de.sciss"      %% "scalacolliderswing-core"  % scalaColliderSwingVersion,
  "de.sciss"      %% "scalacolliderswing-plotting" % scalaColliderSwingVersion,
  // Test
  "org.scalatest" %% "scalatest"                % scalaTestVersion % "test",
  "de.sciss"      %% "topology"                 % topologyVersion  % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

