package com.itegulov.typetheory

import scala.collection.mutable

/**
 * @author Daniyar Itegulov
 */
package object terms {
  def isSameTVar[E](eq: TermEq[E]): Boolean =
    (eq.left, eq.right) match {
      case (TVar(a), TVar(b)) => a == b
      case _ => false
    }

  def isSameTFun[E](eq: TermEq[E]): Boolean =
    (eq.left, eq.right) match {
      case (TFun(a, _), TFun(b, _)) => a == b
      case _ => false
    }

  def isDifferentTFun[E](eq: TermEq[E]): Boolean =
    (eq.left, eq.right) match {
      case (TFun(a, _), TFun(b, _)) => a != b
      case _ => false
    }

  def termReduction[E](eq: TermEq[E]): mutable.Buffer[TermEq[E]] =
    (eq.left, eq.right) match {
      case (TFun(_, arg1), TFun(_, arg2)) => arg1.zip(arg2).map(a => new TermEq(a._1, a._2)).toBuffer
      case _ => throw new IllegalArgumentException("This TermEq can't be reduced")
    }

  def isSwapNeeded[E](eq: TermEq[E]): Boolean =
    eq match {
      case TermEq(TFun(_, _), TVar(_)) => true
      case _ => false
    }

  def swapIfNeeded[E](buffer: mutable.Buffer[TermEq[E]]): mutable.Buffer[TermEq[E]] =
    buffer.map {
      case TermEq(a: TFun[E], b: TVar[E]) => TermEq(b, a)
      case a => a
    }

  def isUnique[E](eq: TermEq[E], buffer: mutable.Buffer[TermEq[E]]): Boolean =
    eq match {
      case t@TermEq(a: TVar[E], b: Term[E]) => b != a && (buffer - eq).exists(_.contains(a))
      case _ => false
    }

  def unify[E](buffer: mutable.Buffer[TermEq[E]]): mutable.Buffer[TermEq[E]] =
    buffer.find(isSameTFun) match {
      case Some(x) => unify(termReduction(x) ++ (buffer - x))
      case None => buffer.find(isDifferentTFun) match {
        case Some(x) => throw new NoSolutionException("No solution")
        case None => buffer.find(isSameTVar) match {
          case Some(x) => unify(buffer.filter(!isSameTVar(_)))
          case None => buffer.find(isSwapNeeded) match {
            case Some(x) => unify(swapIfNeeded(buffer))
            case None => buffer.find(isUnique(_, buffer)) match {
              case Some(x) => x match {
                case t@TermEq(a: TVar[E], b: Term[E]) =>
                  if (b.contains(a)) throw new NoSolutionException("No solution")
                  else unify((buffer - x).map(_.substitute(a, b)) ++ mutable.Buffer(x))
              }
              case None => buffer
            }
          }
        }
      }
    }

  def addIDs[E](buffer: mutable.Buffer[TermEq[E]]): mutable.Buffer[TermEq[E]] = {
    val left: Set[TVar[E]] = buffer.flatMap(_.left.getAllVariables).toSet
    val right: Set[TVar[E]] = buffer.flatMap(_.right.getAllVariables).toSet
    (right -- left).map(v => TermEq(v, v)).toBuffer ++ buffer
  }

  class NoSolutionException(msg: String) extends Exception(msg)
}
