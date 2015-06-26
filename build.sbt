name               := "MutagenTx"

version            := "0.1.0-SNAPSHOT"

organization       := "de.sciss"

scalaVersion       := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.10.5")

description        := "An experiment with genetic algorithms"

homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))

licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"

libraryDependencies ++= Seq(
  "de.sciss"      %% "lucreconfluent"           % "2.11.1",
  "de.sciss"      %% "lucreevent-expr"          % "2.7.4",
  "de.sciss"      %% "lucrestm-bdb"             % "2.1.2",
  "de.sciss"      %% "fileutil"                 % "1.1.1",
  "de.sciss"      %  "prefuse-core"             % "1.0.0",
  "de.sciss"      %% "lucreswing"               % "0.9.1",
  "de.sciss"      %% "processor"                % "0.4.0",
  "de.sciss"      %% "scalacolliderugens-core"  % "1.13.1",
  "de.sciss"      %  "scalacolliderugens-spec"  % "1.13.1",
  "de.sciss"      %% "strugatzki"               % "2.9.0",
  "de.sciss"      %% "filecache-txn"            % "0.3.3",
  "de.sciss"      %% "soundprocesses-core"      % "2.18.1",
  "de.sciss"      %% "kollflitz"                % "0.2.0",
  "com.thoughtworks.xstream" % "xstream" % "1.4.8", // maven central sha1 corruption
  "de.sciss"      %  "weblaf"                   % "1.28",
  "org.scalatest" %% "scalatest"                % "2.2.5" % "test",
  "de.sciss"      %% "topology"                 % "1.0.0" % "test",
  // MFCC and SOM
  "de.sciss"      %% "scissdsp"                 % "1.2.2",
  "de.sciss"      %% "lucredata-core"           % "2.3.2",
  "de.sciss"      %% "lucredata-views"          % "2.3.2",
  "de.sciss"      %% "scalacollider"            % "1.17.2",
  "de.sciss"      %% "scalacolliderswing-core"  % "1.25.1"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

