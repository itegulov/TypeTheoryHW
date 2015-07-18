package com.itegulov.typetheory.lambda

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.{RegexParsers, PackratParsers}
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/**
 * Lexer, telling, that λ isn't a letter
 *
 * @author Daniyar Itegulov
 */
class LambdaLexer extends StdLexical {
  override def letter = elem("letter", c => c.isLetter && c != 'λ')
}

/**
 * Exception, which occurs on unsuccessful parses of lambda expressions
 *
 * @author Daniyar Itegulov
 */
class LambdaParseException(msg: String) extends Exception(msg)

/**
 * Simple lambda parser
 *
 * @author Daniyar Itegulov
 */
/*
object LambdaParser extends StdTokenParsers with PackratParsers {
  type Tokens = LambdaLexer
  val lexical = new LambdaLexer
  lexical.delimiters ++= Seq("λ", ".", "(", ")")

  type P[+T] = PackratParser[T]
  lazy val expr: P[Lambda]             = lambda | application | variable | brackets
  lazy val lambda: P[Abstraction]      = "λ" ~> variable ~ "." ~ expr ^^ { case v ~ "." ~ e  => Abstraction(v, e) }
  lazy val application: P[Application] = expr ~ expr ^^ { case left ~ right => Application(left, right) }
  lazy val variable: P[Variable]       = ident ^^ Variable
  lazy val brackets: P[Lambda]         = "(" ~> expr <~ ")"

  def parse(source: String): Lambda = {
    val tokens = new lexical.Scanner(source)
    phrase(expr)(tokens) match {
      case Success(e, _)  => e
      case err: NoSuccess => throw new LambdaParseException(err.toString)
    }
  }
}
*/
object LambdaParser extends RegexParsers {
  def expression: Parser[Lambda] = application | simpleExpression

  def simpleExpression: Parser[Lambda] = function | variable | "(" ~> expression <~ ")"

  def function: Parser[Abs] = lambda ~> variable ~ "." ~ expression ^^ { case v ~ "." ~ e  => Abs(v, e) }

  def application: Parser[Lambda] =
    simpleExpression~rep1(simpleExpression) ^^ { case exp ~ exps => (exp /: exps) { (app, e) => App(app, e) } }

  def lambda: Parser[String] = """\\|λ""".r

  def variable: Parser[Var] = """[a-z][a-z,0-9,']*""".r ^^ Var

  def apply(input: String): Lambda = {
    parseAll(expression, input) match {
      case Success(e, _) => e
      case err: NoSuccess => throw new LambdaParseException(err.toString)
    }
  }
}