package com.itegulov.typetheory.terms

/**
 * @author Daniyar Itegulov
 */
case class TermEq(left: Term, right: Term) {
  override def toString: String = left + " = " + right

  override def hashCode(): Int = left.hashCode() + right.hashCode() * 31

  override def equals(obj: Any): Boolean = obj match {
    case other: TermEq => other.left == left && other.right == right
    case _ => false
  }

  def substitute(a: TVar, b: Term): TermEq = TermEq(left.substitute(a, b), right.substitute(a, b))

  def contains(what: Term): Boolean = left.contains(what) || right.contains(what)
}
