package com.itegulov.typetheory.lambda

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
object Normalizing {

  case class Redex(v: String, body: Lambda, arg: Lambda)

  private val cache = mutable.WeakHashMap.empty[Redex, Lambda]
  private val variableCache = mutable.Map.empty[String, Var]

  def substitute(lam: Lambda, v: String, e: Lambda, scope: Set[String] = Set()): Lambda = lam match {
    case lamV: Var if v == lamV.name => e
    case v: Var => v
    case App(x, y) =>
      App(substitute(x, v, e, scope), substitute(y, v, e, scope))
    case t@Abs(bind, body) =>
      val bodyFree = body.freeVars.map(_.name)
      if (bind.name == v || !bodyFree.contains(v)) {
        t
      } else {
        val free = getFreeName(bodyFree ++ e.freeVars.map(_.name) ++ scope)
        val freeVar = variableCache.get(free) match {
          case Some(oldV) => oldV
          case None =>
            val freeVar = Var(free)
            variableCache(free) = freeVar
            freeVar
        }
        val sBody = substitute(substitute(body, bind.name, freeVar, scope + free), v, e, scope + free)
        if ((freeVar eq bind) && (sBody eq body)) lam else Abs(freeVar, sBody)
      }
  }

  def getFreeName(used: Set[String], range: Seq[String] = ('a' to 'z').map(_.toString)): String = {
    range.find(c => !used.contains(c)) match {
      case Some(c) => c
      case _ => getFreeName(used, range.map(c => c + "'"))
    }
  }

  def headNormalForm(lam: Lambda, scope: Set[String] = Set()): Lambda = lam match {
    case v: Var => v
    case t@Abs(bind, body) => Abs(bind, headNormalForm(body, scope + bind.name))
    case t@App(lhs, rhs) =>
      val hnf = headNormalForm(lhs, scope)
      hnf match {
        case Abs(bind, body) =>
          val redex = Redex(bind.name, body, rhs)
          cache.get(redex) match {
            case Some(r) => r
            case None =>
              val hnf = headNormalForm(substitute(body, bind.name, rhs, scope + bind.name), scope + bind.name)
              cache(redex) = hnf
              hnf
          }
        case c => App(c, rhs)
      }
  }

  def normalForm(lam: Lambda, scope: Set[String] = Set()): Lambda = lam match {
    case v: Var => v
    case t@Abs(bind, body) =>
      Abs(bind, normalForm(body, scope + bind.name))
    case t@App(lhs, rhs) =>
      headNormalForm(lhs) match {
        case Abs(bind, body) => normalForm(substitute(body, bind.name, rhs, scope + bind.name), scope + bind.name)
        case o => App(normalForm(o), normalForm(rhs))
      }
  }
}
