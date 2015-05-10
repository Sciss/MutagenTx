name               := "MutagenTx"

version            := "0.1.0-SNAPSHOT"

organization       := "de.sciss"

scalaVersion       := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

description        := "An experiment with genetic algorithms"

homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))

licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"

libraryDependencies ++= Seq(
  "de.sciss"      %% "lucreconfluent"           % "2.11.1-SNAPSHOT",
  "de.sciss"      %% "lucreevent-expr"          % "2.7.3",
  "de.sciss"      %% "lucrestm-bdb"             % "2.1.1",
  "de.sciss"      %% "fileutil"                 % "1.1.1",
  "de.sciss"      %  "prefuse-core"             % "1.0.0",
  "de.sciss"      %% "lucreswing"               % "0.9.1",
  "de.sciss"      %% "processor"                % "0.4.0",
  "de.sciss"      %% "scalacolliderugens-core"  % "1.13.1",
  "de.sciss"      %  "scalacolliderugens-spec"  % "1.13.1",
  "de.sciss"      %% "strugatzki"               % "2.9.0",
  "de.sciss"      %% "filecache-txn"            % "0.3.2",
  "de.sciss"      %% "soundprocesses-core"      % "2.18.0",
  "de.sciss"      %% "kollflitz"                % "0.2.0",
  "de.sciss"      %  "weblaf"                   % "1.28",
  "org.scalatest" %% "scalatest"                % "2.2.4" % "test",
  "de.sciss"      %% "topology"                 % "1.0.0" % "test",
  // MFCC and SOM
  "de.sciss"      %% "scissdsp"                 % "1.2.2-SNAPSHOT",
  "de.sciss"      %% "lucredata-core"           % "2.3.1-SNAPSHOT",
  "de.sciss"      %% "lucredata-views"          % "2.3.1-SNAPSHOT"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

