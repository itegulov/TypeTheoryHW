package com.itegulov.typetheory.terms

/**
 * @author Daniyar Itegulov
 */
sealed trait Term {
  def getAllVariables: Set[TVar]
  /**
   * Changes all occurrences of a to b in this term.
   * @param a what to replace
   * @param b on what ot be replaced
   * @return term, with all a's replaced with b's in this term
   */
  def substitute(a: TVar, b: Term): Term

  def contains(a: Term): Boolean
}

case class TFun(name: String, arguments: List[Term]) extends Term {
  override def toString: String = name + "(" + arguments.mkString(", ") + ")"


  override def hashCode(): Int = name.hashCode + 17 * arguments.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case fun: TFun => fun.name == name && fun.arguments == arguments
    case _ => false
  }

  override def getAllVariables: Set[TVar] = (for (x <- arguments) yield x.getAllVariables).flatten.toSet

  /**
   * Changes all occurrences of a to b in this term.
   * @param a what to replace
   * @param b on what ot be replaced
   * @return term, with all a's replaced with b's in this term
   */
  override def substitute(a: TVar, b: Term): Term = TFun(name, arguments.map(_.substitute(a, b)))

  override def contains(a: Term): Boolean = this == a || arguments.exists(_.contains(a))
}

case class TVar(name: String) extends Term {
  override def toString: String = name

  override def hashCode(): Int = name.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case v: TVar => v.name == name
    case _ => false
  }

  override def getAllVariables: Set[TVar] = Set(this)

  /**
   * Changes all occurrences of a to b in this term.
   * @param a what to replace
   * @param b on what ot be replaced
   * @return term, with all a's replaced with b's in this term
   */
  override def substitute(a: TVar, b: Term): Term = if (a == this) b else this

  override def contains(a: Term): Boolean = this == a
}