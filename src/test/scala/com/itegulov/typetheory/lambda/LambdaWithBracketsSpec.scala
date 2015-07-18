package com.itegulov.typetheory.lambda

import org.scalatest.{Matchers, FlatSpec}
import LambdaParser._

/**
 * @author Daniyar Itegulov
 */
class LambdaWithBracketsSpec extends FlatSpec with Matchers {
  "Lambda" should "correctly put brackets in abstractions" in {
    mkStringWithBrackets(apply("λa.λb.a b c (λd.e λf.g) h")) shouldEqual "(λa.(λb.((((a b) c) (λd.(e (λf.g)))) h)))"
  }
}
