package com.itegulov.typetheory

import java.io.PrintWriter

import com.itegulov.typetheory.lambda._

import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW4 extends scala.App {
  val pw = new PrintWriter("task4.out")
  pw.println(normalForm(LambdaParser(Source.fromFile("task4.in", "UTF-8").mkString)))
  pw.close()
}
