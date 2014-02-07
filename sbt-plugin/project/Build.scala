import sbt._
import Keys._


object Dependencies{

  val playVersion = "2.2.1"

  val dependenciesList = Seq(
    "com.typesafe.play" %% "routes-compiler" % playVersion,
    "com.typesafe.play" % "play-exceptions" % playVersion,
    "commons-logging" % "commons-logging" % "1.1.3",
    "org.raml" % "raml-parser" % "0.8.2",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"
  )
}

object Resolvers{
  val resolversList = Seq(
    "typesafe" at "http://repo.typesafe.com/typesafe/releases",
    "mulesoft" at "https://repository-master.mulesoft.org/releases/"
  )
}

object Publish {
  object TargetRepository {
    def scmio: Def.Initialize[Option[sbt.Resolver]] = version { (version: String) =>
      val rootDir = "/srv/maven/"
      val path =
        if (version.trim.endsWith("SNAPSHOT"))
          rootDir + "snapshots/"
        else
          rootDir + "releases/"
      Some(Resolver.sftp("scm.io intern repo", "scm.io", 44144, path))
    }

    def sonatype: Def.Initialize[Option[sbt.Resolver]] = version { (version: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (version.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  }

  lazy val settings = Seq(
    publishMavenStyle := true,
    publishTo <<= TargetRepository.scmio,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    organization := "com.scalableminds",
    organizationName := "scalable minds UG (haftungsbeschränkt) & Co. KG",
    organizationHomepage := Some(url("http://scalableminds.com")),
    startYear := Some(2014),
    description := "Sbt plugin to replace playframeworks integrated routes definitions with RAML files",
    licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/sclableminds/sbt-play-raml")),
    scmInfo := Some(ScmInfo(url("https://github.com/sclableminds/sbt-play-raml"), "https://github.com/scalableminds/sbt-play-raml.git"))
  )
}

object BuildSettings{
  val settings = Seq(
    sbtPlugin := true,
    resolvers := Resolvers.resolversList,
    libraryDependencies ++= Dependencies.dependenciesList
  )
}

object ApplicationBuild extends Build {

  lazy val sbtPlayRaml = Project(
    id = "sbt-play-raml",
    base = file("."),
    settings = Project.defaultSettings ++ BuildSettings.settings ++ Publish.settings)

}