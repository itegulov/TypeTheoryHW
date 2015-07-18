package com.itegulov.typetheory.lambda

import org.scalatest.{Matchers, FlatSpec}

/**
 * @author Daniyar Itegulov
 */
class NormalFormSpec extends FlatSpec with Matchers  {
  "Lambdas" should "correctly convert to normal form" in {
    println(normalForm(LambdaParser("(\\x.x x x) (\\x.x) a b c ((\\x.e e f x e) e)")))
  }
}
