import sbt._
import Keys._

object Sample extends Build {
  
  val sbtPlayRaml = RootProject(file("../../sbt-plugin"))
  
  lazy val root = Project(id = "sample", base = file(".")).dependsOn(sbtPlayRaml)

}