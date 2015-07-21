package com.itegulov.typetheory.terms

/**
 * @author Daniyar Itegulov
 */
sealed trait Term[E] {
  def getAllVariables: Set[TVar[E]]
  /**
   * Changes all occurrences of a to b in this term.
   * @param a what to replace
   * @param b on what ot be replaced
   * @return term, with all a's replaced with b's in this term
   */
  def substitute(a: TVar[E], b: Term[E]): Term[E]

  def contains(a: Term[E]): Boolean
}

case class TFun[E](name: String, arguments: List[Term[E]]) extends Term[E] {
  override def toString: String = name + "(" + arguments.mkString(", ") + ")"


  override def hashCode(): Int = name.hashCode + 17 * arguments.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case fun: TFun[E] => fun.name == name && fun.arguments == arguments
    case _ => false
  }

  override def getAllVariables: Set[TVar[E]] = (for (x <- arguments) yield x.getAllVariables).flatten.toSet

  /**
   * Changes all occurrences of a to b in this term.
   * @param a what to replace
   * @param b on what ot be replaced
   * @return term, with all a's replaced with b's in this term
   */
  override def substitute(a: TVar[E], b: Term[E]): Term[E] = TFun(name, arguments.map(_.substitute(a, b)))

  override def contains(a: Term[E]): Boolean = this == a || arguments.exists(_.contains(a))
}

case class TVar[E](name: E) extends Term[E] {
  override def toString: String = name.toString

  override def hashCode(): Int = name.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case v: TVar[E] => v.name == name
    case _ => false
  }

  override def getAllVariables: Set[TVar[E]] = Set(this)

  /**
   * Changes all occurrences of a to b in this term.
   * @param a what to replace
   * @param b on what ot be replaced
   * @return term, with all a's replaced with b's in this term
   */
  override def substitute(a: TVar[E], b: Term[E]): Term[E] = if (a == this) b else this

  override def contains(a: Term[E]): Boolean = this == a
}