sourceDirectory := file("dummy source directory")

lazy val sharedSettings = MimaSettings.settings ++ Seq(

  name := "scalacheck",

  version := "1.12.5-SNAPSHOT",

  organization := "org.scalacheck",

  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),

  homepage := Some(url("http://www.scalacheck.org")),

  scalaVersion := "2.11.6",

  crossScalaVersions := Seq("2.10.5", "2.11.6"),

  unmanagedSourceDirectories in Compile += (baseDirectory in LocalRootProject).value / "src" / "main" / "scala",

  unmanagedSourceDirectories in Test += (baseDirectory in LocalRootProject).value / "src" / "test" / "scala",

  resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/releases",

  javacOptions += "-Xmx1024M",

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),

  scalacOptions in (Compile,doc) += "-Xfatal-warnings",

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    val (name, path) = if (isSnapshot.value) ("snapshots", "content/repositories/snapshots")
                       else ("releases", "service/local/staging/deploy/maven2")
    Some(name at nexus + path)
  },

  publishMavenStyle := true,

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  pomExtra := {
    <scm>
      <url>https://github.com/rickynils/scalacheck</url>
      <connection>scm:git:git@github.com:rickynils/scalacheck.git</connection>
    </scm>
    <developers>
      <developer>
        <id>rickynils</id>
        <name>Rickard Nilsson</name>
      </developer>
    </developers>
  }
)

import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

lazy val js = project.in(file("js"))
  .settings(sharedSettings: _*)
  .settings(
    previousArtifact := Some("org.scalacheck" % "scalacheck_sjs0.6_2.11" % "1.12.4"),
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )
  .enablePlugins(ScalaJSPlugin)

lazy val jvm = project.in(file("jvm"))
  .settings(sharedSettings: _*)
  .settings(
    previousArtifact := Some("org.scalacheck" % "scalacheck_2.11" % "1.12.4"),
    crossScalaVersions += "2.12.0-M1",
    libraryDependencies += "org.scala-sbt" %  "test-interface" % "1.0"
  )
