package com.itegulov.typetheory

import java.io.PrintWriter

import com.itegulov.typetheory.lambda.LambdaParser
import com.itegulov.typetheory.terms._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW6 extends scala.App {
  val pw = new PrintWriter("task6.out")
  try {
    val (t, freeVars) = LambdaParser(Source.fromFile("task6.in", "UTF-8").mkString).getType
    pw.println(t)
    pw.println(freeVars.mkString("\n"))
  } catch {
    case _: Throwable => pw.println("Лямбда-выражение не имеет типа")
  }
  pw.close()
}
