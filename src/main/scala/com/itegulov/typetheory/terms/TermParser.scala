package com.itegulov.typetheory.terms

import scala.util.parsing.combinator.RegexParsers

/**
 * Exception, which occurs on unsuccessful parses of term expressions
 *
 * @author Daniyar Itegulov
 */
class TermParseException(msg: String) extends Exception(msg)

/**
 * Simple term parser
 *
 * @author Daniyar Itegulov
 */
object TermParser extends RegexParsers {
  def equation: Parser[TermEq[String]] = term ~ "=" ~ term ^^ {case left ~ "=" ~ right => TermEq[String](left, right)}

  def term: Parser[Term[String]] = function | variable

  def function: Parser[TFun[String]] = functionName ~ "(" ~ arguments ~ ")" ^^ {case name ~ "(" ~ args ~ ")" => TFun(name, args)}

  def arguments: Parser[List[Term[String]]] =
    term ~ "," ~ arguments ^^ {case t ~ "," ~ args => t :: args} |
    term ^^ { case t => List(t) }

  def functionName: Parser[String] = """[a-z][a-z0-9']*""".r

  def variable: Parser[TVar[String]] = """[a-z][a-z0-9']*""".r ^^ TVar[String]

  def apply(input: String): TermEq[String] = {
    parseAll(equation, input) match {
      case Success(e, _) => e
      case err: NoSuccess => throw new TermParseException(err.toString)
    }
  }
}