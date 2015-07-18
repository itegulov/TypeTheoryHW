package com.itegulov.typetheory

import java.io.PrintWriter

import com.itegulov.typetheory.lambda._

import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW3 extends scala.App {
  val pw = new PrintWriter("task3.out")
  val string: String = Source.fromFile("task3.in", "UTF-8").mkString
  val parts: Array[String] = string.split( """\[""")
  if (parts.length != 2) throw new IllegalArgumentException("Expected one '['")
  val expression = LambdaParser(parts(0))
  val substituteParts = parts(1).init.split( """:=""")
  if (substituteParts.length != 2) throw new IllegalArgumentException("Expected one ':='")
  val variable: Var = LambdaParser(substituteParts(0)) match {
    case v: Var => v
    case _ => throw new IllegalArgumentException("Expected a variable before :=")
  }
  val exp = LambdaParser(substituteParts(1))
  try {
    pw.println(substitute(expression, variable, exp))
  } catch {
    case e: NoFreeSubstitutionException => pw.println("Нет свободы для подстановки для переменной " + e.v)
  }
  pw.close()
}
