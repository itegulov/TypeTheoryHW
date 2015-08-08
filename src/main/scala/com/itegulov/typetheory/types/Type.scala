package com.itegulov.typetheory.types

import com.itegulov.typetheory.terms.{TFun, TVar, Term}

/**
 * @author Daniyar Itegulov
 */
sealed trait Type {
  def fromType: Term[Type]
  def subType: List[Type]
}

object Type {
  def toType(term: Term[Type]): Type = term match {
    case TVar(a) => a
    case TFun("->", (t1 :: t2 :: Nil)) => Arrow(toType(t1), toType(t2))
  }
}

case class Atom(name: String) extends Type {
  override def toString: String = name

  override def equals(obj: Any): Boolean = obj match {
    case other: Atom => other.name == name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode * 61

  override def fromType: Term[Type] = TVar(this)

  override def subType: List[Type] = List(this)
}

case class Arrow(left: Type, right: Type) extends Type {
  override def toString: String = "(" + left.toString + ") -> (" + right.toString + ")"

  override def equals(obj: Any): Boolean = obj match {
    case other: Arrow => other.left == left && other.right == right
  }

  override def hashCode(): Int = left.hashCode() + right.hashCode() * 67

  override def fromType: Term[Type] = TFun("->", List(left.fromType, right.fromType))

  override def subType: List[Type] = left.subType ++ right.subType
}
