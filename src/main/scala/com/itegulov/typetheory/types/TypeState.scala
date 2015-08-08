package com.itegulov.typetheory.types
import com.itegulov.typetheory.lambda.{Abs, App, Var, Lambda}
import com.itegulov.typetheory.terms.TermEq


/**
 * @author Daniyar Itegulov
 */
case class TypeState(list: List[(Map[Var, Type], Lambda, Type)], terms: List[TermEq[Type]]) {
  def usedTypes: List[Type] = {
    def inTermEq(term: TermEq[Type]) =
      (term.left.getAllVariables ++ term.right.getAllVariables).map(a => Type.toType(a))
    def inTerms = terms.flatMap(a => inTermEq(a)).flatMap(a => a.subType)
    def inTuple(tuple: (Map[Var, Type], Lambda, Type)) = {
      val (map, _, t) = tuple
      t.subType ++ map.values
    }
    (inTerms ++ list.flatMap(inTuple)).distinct
  }

  private def newTypes: Stream[Type] = {
    def newTypesInternal(allTypes: Stream[Type], usedTypes: List[Type]): Stream[Type] = {
      if (usedTypes.contains(allTypes.head)) newTypesInternal(allTypes.tail, usedTypes)
      else allTypes.head #:: newTypesInternal(allTypes.tail, usedTypes)
    }
    newTypesInternal(Lambda.allAtoms, usedTypes)
  }

  def process(): List[TermEq[Type]] = {
    if (list.isEmpty) {
      terms
    } else {
      val (map, lambda, t) = list.head
      lambda match {
        case v: Var => TypeState(list.tail, TermEq(t.fromType, map(v).fromType) :: terms).process()
        case App(l, r) =>
          val newType: Type = newTypes.head
          TypeState((map, l, Arrow(newType, t)) :: (map, r, newType) :: list.tail, terms).process()
        case Abs(v, b) =>
          val newType1: Type = newTypes.head
          val newType2: Type = newTypes.tail.head
          TypeState((map + (v -> newType1), b, newType2) :: list,
            TermEq(t.fromType, Arrow(newType1, newType2).fromType) :: terms).process()
      }
    }
  }
}
