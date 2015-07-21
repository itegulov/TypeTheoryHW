package com.itegulov.typetheory

import java.io.PrintWriter

import com.itegulov.typetheory.terms._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW5 extends scala.App {
  val pw = new PrintWriter("task5.out")
  val buffer: mutable.Buffer[TermEq[String]] = new ArrayBuffer[TermEq[String]]
  for (line <- Source.fromFile("task5.in", "UTF-8").getLines()) {
    buffer += TermParser(line)
  }
  pw.println(addIDs(unify(buffer)).mkString("\n"))
  pw.close()
}
