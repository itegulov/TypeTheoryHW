package com.itegulov.typetheory

import scala.collection.mutable

/**
 * @author Daniyar Itegulov
 */
package object terms {
  def isSameTVar(eq: TermEq): Boolean =
    (eq.left, eq.right) match {
      case (TVar(a), TVar(b)) => a == b
      case _ => false
    }

  def isSameTFun(eq: TermEq): Boolean =
    (eq.left, eq.right) match {
      case (TFun(a, _), TFun(b, _)) => a == b
      case _ => false
    }

  def termReduction(eq: TermEq): mutable.Buffer[TermEq] =
    (eq.left, eq.right) match {
      case (TFun(_, arg1), TFun(_, arg2)) => arg1.zip(arg2).map(a => new TermEq(a._1, a._2)).toBuffer
      case _ => throw new IllegalArgumentException("This TermEq can't be reduced")
    }

  def isSwapNeeded(eq: TermEq): Boolean =
    eq match {
      case TermEq(TFun(_, _), TVar(_)) => true
      case _ => false
    }

  def swapIfNeeded(buffer: mutable.Buffer[TermEq]): mutable.Buffer[TermEq] =
    buffer.transform {
      case TermEq(a@TFun(_, _), b@TVar(_)) => TermEq(b, a)
    }

  def contains(what: Term, where: Term): Boolean =
    where match {
      case v: TVar => v == what
      case f: TFun => f == what || f.arguments.exists(contains(what, _))
    }

  def contains(what: Term, where: TermEq): Boolean =
    contains(what, where.left) || contains(what, where.right)

  def isUnique(eq: TermEq, buffer: mutable.Buffer[TermEq]): Boolean =
    eq match {
      case t@TermEq(a: TVar, b: Term) => b != a && (buffer - eq).exists(contains(a, _))
      case _ => false
    }

  def unify(list: mutable.Buffer[TermEq]): mutable.Buffer[TermEq] =
    list.find(isSameTFun) match {
      case Some(x) => unify(termReduction(x) ++ (list - x))
      case None => list.find(!isSameTFun(_)) match {
        case Some(x) => throw new NoSolutionException("No solution")
        case None => list.find(isSameTVar) match {
          case Some(x) => unify(list.filter(!isSameTVar(_)))
          case None => list.find(isSwapNeeded) match {
            case Some(x) => unify(swapIfNeeded(list))
            case None => list.find(isUnique(_, list)) match {
              case Some(x) => ???
              case None => list
            }
          }
        }
      }
    }

  class NoSolutionException(msg: String) extends Exception(msg)
}
