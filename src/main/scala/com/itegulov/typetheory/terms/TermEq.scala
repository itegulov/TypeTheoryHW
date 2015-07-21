package com.itegulov.typetheory.terms

/**
 * @author Daniyar Itegulov
 */
case class TermEq[E](left: Term[E], right: Term[E]) {
  override def toString: String = left + " = " + right

  override def hashCode(): Int = left.hashCode() + right.hashCode() * 31

  override def equals(obj: Any): Boolean = obj match {
    case other: TermEq[E] => other.left == left && other.right == right
    case _ => false
  }

  def substitute(a: TVar[E], b: Term[E]): TermEq[E] = TermEq(left.substitute(a, b), right.substitute(a, b))

  def contains(what: Term[E]): Boolean = left.contains(what) || right.contains(what)
}
