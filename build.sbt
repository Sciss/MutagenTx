name               := "MutagenTx"

version            := "0.1.0-SNAPSHOT"

organization       := "de.sciss"

scalaVersion       := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

description        := "An experiment with genetic algorithms"

homepage           := Some(url("https://github.com/Sciss/" + name.value))

licenses           := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "lucreconfluent-core" % "2.10.0-SNAPSHOT"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

