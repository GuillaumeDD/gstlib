val scalatest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
val logging_library = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
val slf4j = "org.slf4j" % "slf4j-api" % "1.7.21"
val logback_core = "ch.qos.logback" % "logback-core" % "1.1.7"
val logback_classic = "ch.qos.logback" % "logback-classic" % "1.2.3"
val junit = "junit" % "junit" % "4.12" % "test"
val scalameterCore =  "com.storm-enroute" %% "scalameter-core" % "0.8.2"
val scalameter = "com.storm-enroute" %% "scalameter" % "0.8.2" % "test"


lazy val commonSettings = Seq(
  organization := "com.github.guillaumedd",
  version := "0.1",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.3")
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "gstlib",
    libraryDependencies += scalatest,
    libraryDependencies += logging_library,
    libraryDependencies += slf4j,
    libraryDependencies += logback_core,
    libraryDependencies += logback_classic,
    libraryDependencies += junit,
    libraryDependencies += scalameterCore,
    libraryDependencies += scalameter
  )

scalacOptions ++= Seq("-deprecation", "-Ywarn-unused-import",  "-Ywarn-unused", "-Ywarn-dead-code", "-optimize")

useGpg := true

licenses := Seq("CeCILL-B" -> url("http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html"))

homepage := Some(url("https://github.com/GuillaumeDD/gstlib"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/GuillaumeDD/gstlib"),
    "scm:git@github.com:GuillaumeDD/gstlib.git"
  )
)

developers := List(
  Developer(
    id    = "GuillaumeDD",
    name  = "Guillaume Dubuisson Duplessis",
    email = "gdubuisson@limsi.fr",
    url   = url("http://www.dubuissonduplessis.fr/")
  )
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}


