package com.scalableminds.raml

import play.router.RoutesCompiler._
import scala.util.parsing.input._
import scala.util.matching._
import play.router._
import java.io.File
import org.raml.model.{Action, Resource, Raml}
import org.raml.parser.visitor.RamlDocumentBuilder
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.parsing.combinator._

object RoutesCompiler {

  import scalax.file._


  object RamlParsers extends JavaTokenParsers {

    override def skipWhitespace = false

    override val whiteSpace = """[ \t]+""".r

    override def phrase[T](p: Parser[T]) = new Parser[T] {
      lastNoSuccess = null

      def apply(in: Input) = p(in) match {
        case s@Success(out, in1) =>
          if (in1.atEnd)
            s
          else if (lastNoSuccess == null || lastNoSuccess.next.pos < in1.pos)
            Failure("end of input expected", in1)
          else
            lastNoSuccess
        case _ => lastNoSuccess
      }
    }

    def EOF: util.matching.Regex = "\\z".r

    def namedError[A](p: Parser[A], msg: String): Parser[A] = Parser[A] {
      i =>
        p(i) match {
          case Failure(_, in) => Failure(msg, in)
          case o => o
        }
    }

    def several[T](p: => Parser[T]): Parser[List[T]] = Parser {
      in =>
        import scala.collection.mutable.ListBuffer
        val elems = new ListBuffer[T]
        def continue(in: Input): ParseResult[List[T]] = {
          val p0 = p // avoid repeatedly re-evaluating by-name parser
          @scala.annotation.tailrec
          def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
              case Success(x, rest) => elems += x; applyp(rest)
              case Failure(_, _) => Success(elems.toList, in0)
              case err: Error => err
            }
          applyp(in)
        }
        continue(in)
    }

    def separator: Parser[String] = namedError(whiteSpace, "Whitespace expected")

    def ignoreWhiteSpace: Parser[Option[String]] = opt(whiteSpace)

    // This won't be needed when we upgrade to Scala 2.11, we will then be able to use JavaTokenParser.ident:
    // https://github.com/scala/scala/pull/1466
    def javaIdent: Parser[String] = """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

    def identifier: Parser[String] = namedError(javaIdent, "Identifier expected")

    def end: util.matching.Regex = """\s*""".r

    def comment: Parser[Comment] = "#" ~> ".*".r ^^ {
      case c => Comment(c)
    }

    def newLine: Parser[String] = namedError((("\r" ?) ~> "\n"), "End of line expected")

    def blankLine: Parser[Unit] = ignoreWhiteSpace ~> newLine ^^ {
      case _ => ()
    }

    def parentheses: Parser[String] = {
      "(" ~ (several((parentheses | not(")") ~> """.""".r))) ~ commit(")") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def brackets: Parser[String] = {
      "[" ~ (several((parentheses | not("]") ~> """.""".r))) ~ commit("]") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def string: Parser[String] = {
      "\"" ~ (several((parentheses | not("\"") ~> """.""".r))) ~ commit("\"") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def multiString: Parser[String] = {
      "\"\"\"" ~ (several((parentheses | not("\"\"\"") ~> """.""".r))) ~ commit("\"\"\"") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    def parameterType: Parser[String] = ":" ~> ignoreWhiteSpace ~> rep1sep(identifier, ".") ~ opt(brackets) ^^ {
      case t ~ g => t.mkString(".") + g.getOrElse("")
    }

    def expression: Parser[String] = ((multiString | string | parentheses | brackets | "[^),?=\\n]".r) +) ^^ {
      case p => p.mkString
    }

    def parameterFixedValue: Parser[String] = "=" ~ ignoreWhiteSpace ~ expression ^^ {
      case a ~ _ ~ b => a + b
    }

    def parameterDefaultValue: Parser[String] = "?=" ~ ignoreWhiteSpace ~ expression ^^ {
      case a ~ _ ~ b => a + b
    }

    def parameter: Parser[Parameter] = (identifier <~ ignoreWhiteSpace) ~ opt(parameterType) ~ (ignoreWhiteSpace ~> opt(parameterDefaultValue | parameterFixedValue)) ^^ {
      case name ~ t ~ d => Parameter(name, t.getOrElse("String"), d.filter(_.startsWith("=")).map(_.drop(1)), d.filter(_.startsWith("?")).map(_.drop(2)))
    }

    def parameters: Parser[List[Parameter]] = "(" ~> repsep(ignoreWhiteSpace ~> positioned(parameter) <~ ignoreWhiteSpace, ",") <~ ")"

    // Absolute method consists of a series of Java identifiers representing the package name, controller and method.
    // Since the Scala parser is greedy, we can't easily extract this out, so just parse at least 3
    def absoluteMethod: Parser[List[String]] = namedError(javaIdent ~ "." ~ javaIdent ~ "." ~ rep1sep(javaIdent, ".") ^^ {
      case first ~ _ ~ second ~ _ ~ rest => first :: second :: rest
    }, "Controller method call expected")

    def call: Parser[HandlerCall] = opt("@") ~ absoluteMethod ~ opt(parameters) ^^ {
      case instantiate ~ absMethod ~ parameters => {
        val (packageParts, classAndMethod) = absMethod.splitAt(absMethod.size - 2)
        val packageName = packageParts.mkString(".")
        val className = classAndMethod(0)
        val methodName = classAndMethod(1)
        val dynamic = !instantiate.isEmpty
        HandlerCall(packageName, className, dynamic, methodName, parameters)
      }
    }

    def uriParam: Parser[PathPart] = "{" ~> identifier <~ "}" ^^ {
      case name => DynamicPart(name, "[^/]+", false)
    }

    def staticPathPart: Parser[StaticPart] = ("""[^/\{\}\s]""".r +) ^^ {
      case chars => StaticPart(chars.mkString)
    }

    def pathDelimiter: Parser[StaticPart] = "/" ^^^ StaticPart("/")

    def pathPartParser: Parser[List[PathPart]] =
      ((pathDelimiter | staticPathPart | uriParam) *) ^^ {
        case parts => parts
      }

    def parsePathPart(s: String): ParseResult[List[PathPart]] = pathPartParser(new CharSequenceReader(s))

    def parseCall(s: String) = call(new CharSequenceReader(s))
  }

  def compile(file: File, generatedDir: File, additionalImports: Seq[String], generateReverseRouter: Boolean = true, generateRefReverseRouter: Boolean = true, namespaceReverseRouter: Boolean = false) {

    val pkg = "raml" + Option(Path(file).name).filter(_.endsWith(".raml")).map("." + _.dropRight(".raml".size)).getOrElse("")
    val packageDir = new File(generatedDir, pkg.replace('.', '/'))
    val generated = GeneratedSource(new File(packageDir, "routes_routing.scala"))

    if (generated.needsRecompilation(additionalImports)) {

      val ramlFile = Path(file).toAbsolute
      val ramlContent = ramlFile.string
      val raml = (new RamlDocumentBuilder()).build(ramlContent)
      val routes = collectRoutes(PathPattern(Nil), raml.getResources.asScala)

      generate(ramlFile, Some(pkg), routes, additionalImports, generateReverseRouter, namespaceReverseRouter).foreach {
        item =>
          Path(new File(generatedDir, item._1)).write(item._2)
      }
    }
  }

  import RamlParsers._

  def collectRoutes(path: PathPattern, resources: mutable.Map[String, Resource]): List[Route] = {
    resources.flatMap {
      case (_, resource) =>

        val pathPart = parsePathPart(resource.getRelativeUri) match {
          case Success(parts: List[PathPart], rest) =>
            parts
          case _ =>
            throw new Exception("Failed to parse path")
        }

        val currentPath = PathPattern(path.parts ++ pathPart)
        val paths = resource.getActions.asScala.map {
          case (typ, action) =>
            val call = extractHandlerCall(action)
            Route(HttpVerb(typ.name()), currentPath, call)
        }
        paths ++ collectRoutes(currentPath, resource.getResources.asScala)
    }.toList.map {
      r =>
        r.path.parts match {
          case StaticPart("/") :: tail =>
            r.copy(path = PathPattern(tail))
          case _ =>
            r
        }
    }
  }

  def extractHandlerCall(action: Action): HandlerCall = {
    RamlParsers.parseCall(action.getDescription) match {
      case Success(call: HandlerCall, _) =>
        call
      case _ => throw new Exception("Failed to parse call")
    }
  }

  /**
   * Generate the actual Scala code for this router
   */
  private def generate(file: Path, namespace: Option[String], rules: List[Rule], additionalImports: Seq[String], reverseRouter: Boolean, namespaceReverseRouter: Boolean): Seq[(String, String)] = {

    check(new File(file.path), rules.collect {
      case r: Route => r
    })

    val filePrefix = namespace.map(_.replace('.', '/') + "/").getOrElse("") + "/routes"

    val (path, hash, date) = (file.path.replace(File.separator, "/"), Hash(file, additionalImports), new java.util.Date().toString)
    val routes = rules.collect {
      case r: Route => r
    }

    val files = Seq(filePrefix + "_routing.scala" -> generateRouter(path, hash, date, namespace, additionalImports, rules))
    if (reverseRouter) {
      (files :+ filePrefix + "_reverseRouting.scala" -> generateReverseRouter(path, hash, date, namespace, additionalImports, routes, namespaceReverseRouter)) ++
        generateJavaWrappers(path, hash, date, rules, namespace.filter(_ => namespaceReverseRouter))
    } else {
      files
    }
  }


  /**
   * Precheck routes coherence or throw exceptions early
   */
  private def check(file: java.io.File, routes: List[Route]) {

    routes.foreach {
      route =>

        if (route.call.packageName.isEmpty) {
          throw RoutesCompilationError(
            file,
            "Missing package name",
            Some(route.call.pos.line),
            Some(route.call.pos.column))
        }

        if (route.call.controller.isEmpty) {
          throw RoutesCompilationError(
            file,
            "Missing Controller",
            Some(route.call.pos.line),
            Some(route.call.pos.column))
        }

        route.path.parts.collect {
          case part@DynamicPart(name, regex, _) => {
            route.call.parameters.getOrElse(Nil).find(_.name == name).map {
              p =>
                if (p.fixed.isDefined || p.default.isDefined) {
                  throw RoutesCompilationError(
                    file,
                    "It is not allowed to specify a fixed or default value for parameter: '" + name + "' extracted from the path",
                    Some(p.pos.line),
                    Some(p.pos.column))
                }
                try {
                  java.util.regex.Pattern.compile(regex)
                } catch {
                  case e: Exception => {
                    throw RoutesCompilationError(
                      file,
                      e.getMessage,
                      Some(part.pos.line),
                      Some(part.pos.column))
                  }
                }
            }.getOrElse {
              throw RoutesCompilationError(
                file,
                "Missing parameter in call definition: " + name,
                Some(part.pos.line),
                Some(part.pos.column))
            }
          }
        }

    }
  }
}