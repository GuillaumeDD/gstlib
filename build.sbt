val scalatest = "org.scalatest" %% "scalatest" % "3.1.1" % "test"
val junit = "junit" % "junit" % "4.12" % "test"
val scalameterCore = "com.storm-enroute" %% "scalameter-core" % "0.19" % "test"
val scalameter = "com.storm-enroute" %% "scalameter" % "0.19" % "test"

lazy val commonSettings = Seq(
  organization := "com.github.guillaumedd",
  version := "0.1.2",
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.1")
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "gstlib",
    libraryDependencies += scalatest,
    libraryDependencies += junit,
    libraryDependencies += scalameterCore,
    libraryDependencies += scalameter
  )

scalacOptions ++= {
  Seq("-deprecation", "-Ywarn-unused", "-Ywarn-dead-code") ++
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 12 => Seq("-opt:l:inline", "-opt-inline-from:**", "-Ywarn-unused:imports")
      case _ => Seq("-optimize", "-Ywarn-unused-import")
    })
}

unmanagedSourceDirectories in Compile ++= {
  (unmanagedSourceDirectories in Compile).value.map { dir =>
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => file(dir.getPath ++ "-2.13+")
      case _ => file(dir.getPath ++ "-2.13-")
    }
  }
}

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
    id = "GuillaumeDD",
    name = "Guillaume Dubuisson Duplessis",
    email = "gdubuisson@limsi.fr",
    url = url("http://www.dubuissonduplessis.fr/")
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
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}


