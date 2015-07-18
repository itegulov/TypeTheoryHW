package com.itegulov.typetheory.lambda

import org.scalatest.{Matchers, FlatSpec}
import LambdaParser._

/**
 * @author Daniyar Itegulov
 */
class LambdaFreeVarsSpec extends FlatSpec with Matchers {
  "Lambdas" should "correctly get free variables" in {
    apply("a").freeVars shouldEqual Set(Var("a"))
    apply("λa.a").freeVars shouldEqual Set()
    apply("λa.λb.a b c (λd.e λf.g) h").freeVars shouldEqual Set(Var("e"), Var("g"), Var("c"), Var("h"))
  }
}
