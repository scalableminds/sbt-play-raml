import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {
  val appName         = "play-raml-sample"
  val appVersion      = "0.1-SNAPSHOT"

  val dependencies = Seq()

  val main = play.Project(appName, appVersion, dependencies).settings(
    organization := "com.scalableminds",
    organizationName := "scalable minds UG (haftungsbeschränkt) & Co. KG",
    organizationHomepage := Some(new URL("http://scm.io"))
  )
}