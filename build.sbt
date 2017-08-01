val scalatest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
val logging_library = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
val slf4j = "org.slf4j" % "slf4j-api" % "1.7.21"
val logback_core = "ch.qos.logback" % "logback-core" % "1.1.7"
val logback_classic = "ch.qos.logback" % "logback-classic" % "1.2.3"
val junit = "junit" % "junit" % "4.12" % "test"
val scalameterCore =  "com.storm-enroute" %% "scalameter-core" % "0.8.2"
val scalameter = "com.storm-enroute" %% "scalameter" % "0.8.2" % "test"


lazy val commonSettings = Seq(
  organization := "fr.limsi",
  version := "2017.1-alpha",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.3")
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "generalizedSuffixTreeLib",
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
