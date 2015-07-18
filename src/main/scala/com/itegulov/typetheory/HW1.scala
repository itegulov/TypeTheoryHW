package com.itegulov.typetheory

import java.io.PrintWriter

import com.itegulov.typetheory.lambda._

import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW1 extends scala.App {
  val pw = new PrintWriter("task1.out")
  pw.println(mkStringWithBrackets(LambdaParser(Source.fromFile("task1.in", "UTF-8").mkString)))
  pw.close()
}
