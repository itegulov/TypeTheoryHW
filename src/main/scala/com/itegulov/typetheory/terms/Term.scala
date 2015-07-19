package com.itegulov.typetheory.terms

/**
 * @author Daniyar Itegulov
 */
sealed trait Term {

}

case class TFun(name: String, arguments: List[Term]) extends Term {
  override def toString: String = name + "(" + arguments.mkString(", ") + ")"


  override def hashCode(): Int = name.hashCode + 17 * arguments.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case fun: TFun => fun.name == name && fun.arguments == arguments
    case _ => false
  }
}

case class TVar(name: String) extends Term {
  override def toString: String = name

  override def hashCode(): Int = name.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case v: TVar => v.name == name
    case _ => false
  }
}