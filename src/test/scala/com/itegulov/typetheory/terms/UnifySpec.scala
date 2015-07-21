package com.itegulov.typetheory.terms

import org.scalatest.{Matchers, FlatSpec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
 * @author Daniyar Itegulov
 */
class UnifySpec extends FlatSpec with Matchers {
  "Unifier" should "correctly unify" in {
    val system: mutable.Buffer[TermEq[String]] = ArrayBuffer(
      TermParser("g(x2)=x1"),
      TermParser("f(x1,h(x1),x2)=f(g(x3),x4,x3)")
    )
    addIDs(unify(system)) shouldEqual ArrayBuffer(
      TermEq[String](TVar[String]("x2"), TVar[String]("x2")),
      TermEq[String](TVar[String]("x4"), TFun[String]("h", List(TFun[String]("g", List(TVar[String]("x2")))))),
      TermEq[String](TVar[String]("x1"), TFun[String]("g", List(TVar[String]("x2")))),
      TermEq[String](TVar[String]("x3"), TVar[String]("x2"))
    )
  }
}
