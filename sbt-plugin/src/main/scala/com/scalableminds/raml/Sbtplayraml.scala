package com.scalableminds.raml

import sbt._
import Keys._
import play.api._
import play.router.RoutesCompiler.{RoutesCompilationError, GeneratedSource}

object Sbtplayraml extends Plugin with RoutesCompilationExceptions{

  val routesImport = SettingKey[Seq[String]]("play-routes-imports")

  val generateReverseRouter = SettingKey[Boolean]("play-generate-reverse-router",
    "Whether the reverse router should be generated. Setting to false may reduce compile times if it's not needed.")

  val generateRefReverseRouter = SettingKey[Boolean]("play-generate-ref-reverse-router",
    "Whether the ref reverse router should be generated along with reverse router. Setting to false will make it easy to export routes to other projects and improve compile time.")

  val namespaceReverseRouter = SettingKey[Boolean]("play-namespace-reverse-router",
    "Whether the reverse router should be namespaced. Useful if you have many routers that use the same actions.")

  val confDirectory = SettingKey[File]("play-conf")

  override lazy val settings = Seq(

    routesImport ++= Seq(
      "play.libs.F"
    ),

    sourceGenerators in Compile <+= (state, confDirectory, sourceManaged in Compile, routesImport) map {
      (s, cd, sm, ri) => RouteFiles(s, Seq(cd), sm, ri, true, true, false)
    }
  )

  private val RouteFiles = (state: State, sourceDirectories: Seq[File], generatedDir: File, additionalImports: Seq[String], reverseRouter: Boolean, reverseRefRouter: Boolean, namespaceReverseRouter: Boolean) => {

    val javaRoutes = (generatedDir ** "routes.java")
    val scalaRoutes = (generatedDir ** "routes_*.scala")
    (javaRoutes.get ++ scalaRoutes.get).map(GeneratedSource(_)).foreach(_.sync())
    println("Source: " + sourceDirectories.mkString + " Result: " + (sourceDirectories * "*.raml"))
    try {
      { (sourceDirectories * "*.raml").get ++ (sourceDirectories * "raml").get }.map { routesFile =>
        RoutesCompiler.compile(routesFile, generatedDir, additionalImports, reverseRouter, reverseRefRouter, namespaceReverseRouter)
      }
    } catch {
      case RoutesCompilationError(source, message, line, column) => {
        throw reportCompilationError(state, RoutesCompilationException(source, message, line, column.map(_ - 1)))
      }
    }

    (scalaRoutes.get ++ javaRoutes.get).map(_.getAbsoluteFile)

  }

  def reportCompilationError(state: State, error: RoutesCompilationException) = {
    val log = state.log
    // log the source file and line number with the error message
    log.error(Option(error.sourceName).getOrElse("") + Option(error.line).map(":" + _).getOrElse("") + ": " + error.getMessage)
    Option(error.interestingLines(0)).map(_.focus).flatMap(_.headOption) map { line =>
      // log the line
      log.error(line)
      Option(error.position).map { pos =>
        // print a carat under the offending character
        val spaces = (line: Seq[Char]).take(pos).map {
          case '\t' => '\t'
          case x => ' '
        }
        log.error(spaces.mkString + "^")
      }
    }
    error
  }
}

trait RoutesCompilationExceptions{
  case class RoutesCompilationException(source: File, message: String, atLine: Option[Int], column: Option[Int]) extends PlayException.ExceptionSource(
    "Compilation error", message) with FeedbackProvidedException {
    def line = atLine.map(_.asInstanceOf[java.lang.Integer]).orNull
    def position = column.map(_.asInstanceOf[java.lang.Integer]).orNull
    def input = scalax.file.Path(source).string
    def sourceName = source.getAbsolutePath
  }
}
