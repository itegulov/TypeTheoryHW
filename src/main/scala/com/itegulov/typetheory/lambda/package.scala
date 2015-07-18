package com.itegulov.typetheory

/**
 * @author Daniyar Itegulov
 */
package object lambda {
  private def mkStringWithBrackets(expression: Lambda, sb: StringBuilder): Unit = {
    expression match {
      case variable: Var => sb.append(variable.name)
      case application: App =>
        sb.append("(")
        mkStringWithBrackets(application.function, sb)
        sb.append(" ")
        mkStringWithBrackets(application.argument, sb)
        sb.append(")")
      case abstraction: Abs =>
        sb.append("(λ").append(abstraction.variable.name).append(".")
        mkStringWithBrackets(abstraction.body, sb)
        sb.append(")")
    }
  }

  def mkStringWithBrackets(expression: Lambda): String = {
    val sb = new StringBuilder
    mkStringWithBrackets(expression, sb)
    sb.mkString
  }

  def substitute(expression: Lambda, variable: Var, toSubstitute: Lambda): Lambda = {
    expression.substitute(variable, toSubstitute) match {
      case Right(e) => e
      case Left(t) => throw new NoFreeSubstitutionException(t)
    }
  }

  class NoFreeSubstitutionException(val v: Var) extends Exception
}
