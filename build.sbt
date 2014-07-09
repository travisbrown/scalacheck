name := "scalacheck"

version := "1.8"

organization := "org.scalacheck"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("http://www.scalacheck.org"))

scalaVersion := "2.11.0"

crossScalaVersions := Seq("2.9.3", "2.10.4", "2.11.0")

resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scala-tools.testing" %  "test-interface" % "0.5"

libraryDependencies <++= (scalaVersion){sVer =>
  sVer match {
    case "2.10.4" => Seq("org.scala-lang" % "scala-actors" % sVer)
    case "2.11.0" => Seq("org.scala-lang" % "scala-actors" % sVer, "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1")
    case _ => Seq()
  }
}

javacOptions ++= Seq("-Xmx1024M")

scalacOptions += "-deprecation"

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <scm>
    <url>git@github.com:rickynils/scalacheck.git</url>
    <connection>scm:git:git@github.com:rickynils/scalacheck.git</connection>
  </scm>
  <developers>
    <developer>
      <id>rickynils</id>
      <name>Rickard Nilsson</name>
      <url>http://www.scalacheck.org</url>
    </developer>
  </developers>
)
