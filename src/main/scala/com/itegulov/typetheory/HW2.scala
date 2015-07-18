package com.itegulov.typetheory

import java.io.PrintWriter

import com.itegulov.typetheory.lambda.{Var, LambdaParser}

import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW2 extends scala.App {
  val pw = new PrintWriter("task2.out")
  val freeVars = LambdaParser(Source.fromFile("task2.in", "UTF-8").mkString).freeVars.toArray.sorted(new Ordering[Var] {
    override def compare(x: Var, y: Var): Int = x.name.compareTo(y.name)
  })
  pw.println(freeVars.mkString("\n"))
  pw.close()
}
