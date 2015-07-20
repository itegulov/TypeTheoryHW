package com.itegulov.typetheory.terms

import org.scalatest.{Matchers, FlatSpec}

import scala.collection.mutable.ArrayBuffer

/**
 * @author Daniyar Itegulov
 */
class UnifySpec extends FlatSpec with Matchers {
  "Unifier" should "correctly unify" in {
    addIDs(unify(ArrayBuffer(
      TermEq(TFun("g", List(TVar("x2"))), TVar("x1")),
      TermEq(TFun("f", List(TVar("x1"), TFun("h", List(TVar("x1"))), TVar("x2"))),
             TFun("f", List(TFun("g", List(TVar("x3"))), TVar("x4"), TVar("x3"))))
    ))) shouldEqual ArrayBuffer(
      TermEq(TVar("x2"), TVar("x2")),
      TermEq(TVar("x4"), TFun("h", List(TFun("g", List(TVar("x2")))))),
      TermEq(TVar("x1"), TFun("g", List(TVar("x2")))),
      TermEq(TVar("x3"), TVar("x2"))
    )
  }
}