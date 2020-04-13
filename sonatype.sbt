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
    email = "guillaume@dubuissonduplessis.fr",
    url = url("http://www.dubuissonduplessis.fr/")
  )
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("GuillaumeDD", "gstlib", "guillaume@dubuissonduplessis.fr"))

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}