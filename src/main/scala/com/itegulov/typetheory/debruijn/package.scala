package com.itegulov.typetheory

/**
 * @author Daniyar Itegulov
 */
package object debruijn {
  def whnf(exp: DeBruijn): DeBruijn = {
    exp match {
      case o@DApp(DAbs(_), _) => whnf(o.reduce)
      case DApp(a, b) => whnf(a) match {
        case abs: DAbs => whnf(DApp(abs, b).reduce)
        case e: DeBruijn => DApp(e, b)
      }
      case e: DeBruijn => e
    }
  }

  def normalForm(exp: DeBruijn): DeBruijn = {
    whnf(exp) match {
      case v: DVar => v
      case DAbs(a) => DAbs(normalForm(a))
      case DApp(a, b) => DApp(normalForm(a), normalForm(b))
    }
  }
}
