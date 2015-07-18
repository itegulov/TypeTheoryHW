package com.itegulov.typetheory.lambda

import com.itegulov.typetheory.lambda.LambdaParser._
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Daniyar Itegulov
 */

class LambdaSubstituteSpec extends FlatSpec with Matchers {
  "Lambdas" should "correctly get free variables" in {
    substitute(apply("a"), Var("a"), apply("b c")) shouldEqual apply("b c")
    substitute(apply("λa.(a b)"), Var("b"), apply("b c")) shouldEqual apply("λa.(a (b c))")
    intercept[NoFreeSubstitutionException] {
      substitute(apply("λx.x f z λf.f λz.z f f"), Var("f"), apply("λg.z x")) shouldEqual apply("λa.(a (b c))")
    }
    intercept[NoFreeSubstitutionException] {
      substitute(apply("λa.a"), Var("a"), apply("b"))
    }
  }
}