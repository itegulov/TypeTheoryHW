package com.itegulov.typetheory.lambda

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Daniyar Itegulov
 */
class NormalFormSpec extends FlatSpec with Matchers  {
  "Lambdas" should "correctly convert to normal form" in {
    normalForm(LambdaParser("(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\f.\\x.f (f (f x)))")) shouldEqual
      LambdaParser("(\\a.(\\b.(a (a b))))")
    normalForm(LambdaParser("(\\x.x x x) (\\x.x) a b c ((\\x.e e f x e) e)")) shouldEqual
      LambdaParser("(((a b) c) ((((e e) f) e) e))")
    normalForm(LambdaParser("(\\y.\\m.y (\\f.\\n.(\\s.(s (\\x.\\a.\\b.b) (\\a.\\b.a)) (\\f.\\x.x) (f s)) (m n))" +
      "(\\f.\\x.f (f (f x)))) (\\f.(\\x.f (x x)) (\\x.f (x x))) " +
      "((\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)))")) shouldEqual
      LambdaParser("\\a.\\b.b")
  }
}
