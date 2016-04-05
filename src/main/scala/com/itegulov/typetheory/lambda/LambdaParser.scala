package com.itegulov.typetheory.lambda

import scala.collection.mutable

/**
  * Exception, which occurs on unsuccessful parses of lambda expressions
  *
  * @author Daniyar Itegulov
  */
class LambdaParseException extends Exception

/**
  * Simple lambda parser
  *
  * @author Daniyar Itegulov
  */
object LambdaParser {
  def apply(s: String): Lambda = {
    var cPos = 0
    val reused: mutable.Map[Lambda, Lambda] = mutable.Map[Lambda, Lambda]()
    val reusedVar: mutable.Map[String, Var] = mutable.Map[String, Var]()
    def spaces(): Unit = while (cPos < s.length() && Character.isWhitespace(s.charAt(cPos))) cPos += 1
    def reuseVar(name: String): Var = reusedVar.getOrElseUpdate(name, Var(name))
    def reuse(lam: Lambda): Lambda = reused.getOrElseUpdate(lam, lam)
    def lexeme(): String = {
      val begin = cPos
      while (cPos < s.length() && (Character.isLetterOrDigit(s.charAt(cPos)) || s.charAt(cPos) == '\'')) cPos += 1
      s.substring(begin, cPos)
    }
    def recursive(): Lambda = {
      var result: Option[Lambda] = None
      while (cPos < s.length()) {
        val c = s.charAt(cPos)
        c match {
          case '\\' =>
            cPos += 1
            spaces()
            val p = lexeme()
            if (cPos == s.length() || s.charAt(cPos) != '.') throw new LambdaParseException()
            cPos += 1
            val res = reuse(Abs(reuseVar(p), recursive()))
            return res
          case '(' =>
            cPos += 1
            val nested = recursive()
            result = result match {
              case None => Some(nested)
              case Some(r) => Some(reuse(App(r, nested)))
            }
          case ')' =>
            cPos += 1
            return result match {
              case None => throw new LambdaParseException()
              case Some(r) => r
            }
          case _ if c.isLetter || c.isDigit || c == '\'' =>
            val name = lexeme()
            result = result match {
              case None => Some(reuseVar(name))
              case Some(r) => Some(reuse(App(r, reuseVar(name))))
            }
          case _ =>
            if (Character.isWhitespace(s.charAt(cPos)))
              spaces()
            else
              throw new LambdaParseException()
        }
      }
      result.getOrElse(throw new LambdaParseException())
    }
    recursive()
  }
}