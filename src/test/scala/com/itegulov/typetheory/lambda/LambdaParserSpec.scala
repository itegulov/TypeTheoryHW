package com.itegulov.typetheory.lambda

import org.scalatest.{Matchers, FlatSpec}
import LambdaParser._

/**
 * @author Daniyar Itegulov
 */
class LambdaParserSpec extends FlatSpec with Matchers {
  "Lambda parser" should "correctly apply variables" in {
    apply("a") shouldEqual Var("a")
    apply("abacaba") shouldEqual Var("abacaba")
    apply("a1") shouldEqual Var("a1")
    apply("a'") shouldEqual Var("a'")
    apply("a1'") shouldEqual Var("a1'")
    apply("ab'") shouldEqual Var("ab'")
    apply("a'1") shouldEqual Var("a'1")
    apply("a'b") shouldEqual Var("a'b")
    apply("abcdefghijklmnopqrstuvwxyz0123456789") shouldEqual Var("abcdefghijklmnopqrstuvwxyz0123456789")
  }

  def expectException(string: String): Unit = {
    intercept[LambdaParseException] {
      apply(string)
    }
  }

  it should "produce LambdaParseException when irregular variable names are specified" in {
    expectException("1")
    expectException("1a")
    expectException("λ")
    expectException("a[")
    expectException("[a")
    expectException("a.")
    expectException(".a")
    expectException("a(")
    expectException("(a")
    expectException("a)")
    expectException(")a")
    expectException("'a")
  }

  "Lambda parser" should "correctly react on variables in brackets" in {
    apply("(a)") shouldEqual Var("a")
    apply("(abacaba)") shouldEqual Var("abacaba")
    apply("(a1)") shouldEqual Var("a1")
    apply("(abcdefghijklmnopqrstuvwxyz123456789)") shouldEqual Var("abcdefghijklmnopqrstuvwxyz123456789")
  }

  "Lambda parser" should "correctly apply application" in {
    apply("a b") shouldEqual App(Var("a"), Var("b"))
    apply("a (b)") shouldEqual App(Var("a"), Var("b"))
    apply("(a) b") shouldEqual App(Var("a"), Var("b"))
    apply("(a b)") shouldEqual App(Var("a"), Var("b"))
    apply("a b c") shouldEqual App(App(Var("a"), Var("b")), Var("c"))
    apply("(a b) (c d)") shouldEqual App(App(Var("a"), Var("b")), App(Var("c"), Var("d")))
  }
}
