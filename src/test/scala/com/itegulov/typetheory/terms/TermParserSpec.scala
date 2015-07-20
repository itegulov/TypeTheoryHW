package com.itegulov.typetheory.terms

import org.scalatest.{Matchers, FlatSpec}

/**
 * @author Daniyar Itegulov
 */
class TermParserSpec extends FlatSpec with Matchers {

  "TermParser" should "correctly parse equations" in {
    TermParser("h(x1,p2,g3)=g(x,y)") shouldEqual TermEq(
      TFun("h", List(TVar("x1"), TVar("p2"), TVar("g3"))),
      TFun("g", List(TVar("x"), TVar("y")))
    )
  }
}
