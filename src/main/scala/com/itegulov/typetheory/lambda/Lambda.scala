package com.itegulov.typetheory.lambda

import com.itegulov.typetheory.types.{TypeState, Arrow, Atom, Type}
import com.itegulov.typetheory.terms._

import scala.collection.mutable

/**
 * @author Daniyar Itegulov
 */
object Lambda {
  private def atoms(n: Int): Stream[Type] = Atom("τ" + n) #:: atoms(n + 1)

  def allAtoms: Stream[Type] = atoms(0)
}


sealed trait Lambda {
  def freeVars: Set[Var]

  def boundVars: Set[Var]

  def substitute(v: Var, e: Lambda): Either[Var, Lambda]

  def getFullType: (Type, Map[Var, Type], mutable.Buffer[TermEq[Type]]) = {
    val vars: List[Var] = freeVars.toList
    val allType: List[Type] = Lambda.allAtoms.take(vars.length).toList
    val map: Map[Var, Type] = vars.zip(allType).toMap
    val finalType: Type = Lambda.allAtoms(vars.length)
    (finalType, map, TypeState(List((map, this, finalType)), List()).process().toBuffer)
  }

  def getType: (Type, Map[Var, Type]) = {
    def swap(terms: mutable.Buffer[TermEq[Type]]): mutable.Buffer[TermEq[Type]] = terms.map {
      // TODO: add comparing of types
      case TermEq(x@(TVar(a)), y@(TVar(b))) if a > b => TermEq(y, x)
      case a => a
    }
    def find(t: Type, terms: mutable.Buffer[TermEq[Type]]): Option[Term[Type]] =
      terms.find {
        case TermEq(TVar(a), _) => a == t
        case _ => false
      } match {
        case Some(term) => Some(term.right)
        case None => None
      }
    def toType(term: Term[Type]): Type =
      term match {
        case TVar(a) => a
        case TFun("->", (l::r::Nil)) => Arrow(toType(l), toType(r))
      }
    val (t0, fv, termEqs): (Type, Map[Var, Type], mutable.Buffer[TermEq[Type]]) = getFullType
    val ns: mutable.Buffer[TermEq[Type]] = addIDs(unify(swap(unify(termEqs))))
    val ans: Option[Term[Type]] = find(t0, ns)
    val freeAns: Map[Var, Type] = fv.map {
      case (l, t) => (l, toType(find(t, ns).get))
    }
    (toType(ans.get), freeAns)
  }
}

case class Var(name: String) extends Lambda {
  override def hashCode(): Int = name.hashCode

  override def equals(other: Any) = other match {
    case other: Var => other.name == name
    case _ => false
  }

  override def toString = name

  override def freeVars: Set[Var] = Set(this)

  override def boundVars: Set[Var] = Set()

  override def substitute(v: Var, e: Lambda): Either[Var, Lambda] = Right(if (this == v) e else this)
}

case class App(function: Lambda, argument: Lambda) extends Lambda {


  override def hashCode(): Int = function.hashCode() * 17 + argument.hashCode()

  override def equals(other: Any) = other match {
    case other: App => other.function == function && other.argument == argument
    case _ => false
  }

  override def toString = mkStringWithBrackets(this)

  override def freeVars: Set[Var] = function.freeVars ++ argument.freeVars

  override def boundVars: Set[Var] = function.boundVars ++ argument.boundVars

  override def substitute(v: Var, e: Lambda): Either[Var, Lambda] = (function.substitute(v, e), argument.substitute(v, e)) match {
    case (Right(l), Right(r)) => Right(App(l, r))
    case (Left(t), _) => Left(t)
    case (_, Left(t)) => Left(t)
  }
}

case class Abs(variable: Var, body: Lambda) extends Lambda {

  override def hashCode(): Int = variable.hashCode() * 37 + body.hashCode()

  override def equals(other: Any) = other match {
    case other: Abs => other.variable == variable && other.body == body
    case _ => false
  }

  override def toString = mkStringWithBrackets(this)

  override def freeVars: Set[Var] = body.freeVars - variable

  override def boundVars: Set[Var] = body.boundVars + variable

  override def substitute(v: Var, e: Lambda): Either[Var, Lambda] =
    if (variable == v || e.freeVars.contains(variable))
      Left(variable)
    else
      body.substitute(v, e) match {
        case Right(in) => Right(Abs(variable, in))
        case Left(t) => Left(t)
      }
}
