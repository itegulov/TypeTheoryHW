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

  def isDifferentTFun(eq: TermEq): Boolean =
    (eq.left, eq.right) match {
      case (TFun(a, _), TFun(b, _)) => a != b
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
    buffer.map {
      case TermEq(a: TFun, b: TVar) => TermEq(b, a)
      case a => a
    }

  def isUnique(eq: TermEq, buffer: mutable.Buffer[TermEq]): Boolean =
    eq match {
      case t@TermEq(a: TVar, b: Term) => b != a && (buffer - eq).exists(_.contains(a))
      case _ => false
    }

  def unify(buffer: mutable.Buffer[TermEq]): mutable.Buffer[TermEq] =
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
                case t@TermEq(a: TVar, b: Term) =>
                  if (b.contains(a)) throw new NoSolutionException("No solution")
                  else unify((buffer - x).map(_.substitute(a, b)) ++ mutable.Buffer(x))
              }
              case None => buffer
            }
          }
        }
      }
    }

  def addIDs(buffer: mutable.Buffer[TermEq]): mutable.Buffer[TermEq] = {
    val left: Set[TVar] = buffer.flatMap(_.left.getAllVariables).toSet
    val right: Set[TVar] = buffer.flatMap(_.right.getAllVariables).toSet
    (right -- left).map(v => TermEq(v, v)).toBuffer ++ buffer
  }

  class NoSolutionException(msg: String) extends Exception(msg)
}
