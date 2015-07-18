package com.itegulov.typetheory.conversion

import com.itegulov.typetheory.debruijn._
import com.itegulov.typetheory.lambda.LambdaParser._
import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Daniyar Itegulov
 */
class ConversionSpec extends FlatSpec with Matchers {
  "Conversion package" should "correctly convert lambdas to DeBruijn" in {
    removeNames(apply("λx.x")) shouldEqual DAbs(DVar(1))
    removeNames(apply("λx.λy.x (y x)")) shouldEqual DAbs(DAbs(DApp(DVar(2), DApp(DVar(1), DVar(2)))))
    removeNames(apply("λx.x y")) shouldEqual DAbs(DApp(DVar(1),DVar(26)))
  }

  "Conversion package" should "correctly convert DeBruijn to lambdas" in {
    addNames(DAbs(DVar(1))) shouldEqual apply("λa.a")
    addNames(DAbs(DAbs(DApp(DVar(2), DApp(DVar(1), DVar(2)))))) shouldEqual apply("λa.λb.a (b a)")
    addNames(DAbs(DApp(DVar(1),DVar(26)))) shouldEqual apply("λa.a y")
  }
}
