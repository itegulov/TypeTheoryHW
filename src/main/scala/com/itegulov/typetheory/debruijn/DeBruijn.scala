package com.itegulov.typetheory.debruijn

/**
 * @author Daniyar Itegulov
 */
sealed trait DeBruijn {
  private[debruijn] def freeVars(depth: Int): Set[DVar]

  def freeVars: Set[DVar] = {
    freeVars(0)
  }

  private[debruijn] def substitute(expression: DeBruijn, depth: Int): DeBruijn

  def substitute(expression: DeBruijn): DeBruijn = {
    substitute(expression, 0)
  }

  private[debruijn] def changeFree(depth: Int, function: (Int => Int)): DeBruijn

  def incrementFree: DeBruijn = changeFree(0, i => i + 1)

  def decrementFree: DeBruijn = changeFree(0, i => i - 1)

  def reduce: DeBruijn = this
}

case class DVar(num: Int) extends DeBruijn {
  override private[debruijn] def freeVars(depth: Int): Set[DVar] = if (num > depth) Set(this) else Set()

  override private[debruijn] def substitute(expression: DeBruijn, depth: Int): DeBruijn =
    if (depth == num) expression else this

  override private[debruijn] def changeFree(d: Int, f: (Int) => Int): DeBruijn = if (num > d) DVar(f(num)) else this
}

case class DApp(func: DeBruijn, arg: DeBruijn) extends DeBruijn {
  override private[debruijn] def freeVars(depth: Int): Set[DVar] = func.freeVars(depth) ++ arg.freeVars(depth)

  override private[debruijn] def substitute(expression: DeBruijn, depth: Int): DeBruijn =
    DApp(func.substitute(expression, depth), arg.substitute(expression, depth))

  override private[debruijn] def changeFree(depth: Int, function: (Int) => Int): DeBruijn =
    DApp(func.changeFree(depth, function), arg.changeFree(depth, function))

  override def reduce: DeBruijn = {
    func match {
      case abs: DAbs => abs.substitute(arg).asInstanceOf[DAbs].exp.decrementFree
      case _ => this
    }
  }
}

case class DAbs(exp: DeBruijn) extends DeBruijn {
  override private[debruijn] def freeVars(depth: Int): Set[DVar] = exp.freeVars(depth + 1)

  override private[debruijn] def substitute(expression: DeBruijn, depth: Int): DeBruijn =
    DAbs(exp.substitute(expression.incrementFree, depth + 1))

  override private[debruijn] def changeFree(depth: Int, function: (Int) => Int): DeBruijn =
    DAbs(exp.changeFree(depth + 1, function))
}

