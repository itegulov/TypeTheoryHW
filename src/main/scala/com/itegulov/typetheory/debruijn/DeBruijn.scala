package com.itegulov.typetheory.debruijn

/**
 * @author Daniyar Itegulov
 */
sealed trait DeBruijn {
  private[debruijn] def freeVars(depth: Int): Set[DVar]
  def freeVars: Set[DVar] = {
    freeVars(0)
  }
}

case class DVar(num: Int) extends DeBruijn {
  override private[debruijn] def freeVars(depth: Int): Set[DVar] = if (num > depth) Set(this) else Set()
}

case class DApp(func: DeBruijn, arg: DeBruijn) extends DeBruijn {
  override private[debruijn] def freeVars(depth: Int): Set[DVar] = func.freeVars(depth) ++ arg.freeVars(depth)
}

case class DAbs(exp: DeBruijn) extends DeBruijn {
  override private[debruijn] def freeVars(depth: Int): Set[DVar] = exp.freeVars(depth + 1)
}

