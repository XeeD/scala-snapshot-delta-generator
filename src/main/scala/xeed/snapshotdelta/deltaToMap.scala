package xeed.snapshotdelta

import shapeless.{ Poly2 }
import shapeless.PolyDefns.Case

object deltaToMap extends Poly2 {
  implicit def caseObjectDiff[E](implicit deltaCalc: ValueDeltaCalc[E]): Case.Aux[List[ValueDiff[Any]], ObjectDiff[E], List[ValueDiff[Any]]] = at((acc, delta) => delta match {
    case IdenticalField(_) => acc
    case DifferentField(a, b, _) => deltaCalc(a, b) :: acc
  })
//  implicit def caseIdentical: Case.Aux[Int, IdenticalField[_], Int] = at((acc, _i) => acc)
//  implicit def caseDifferent: Case.Aux[Int, DifferentField[_, _], Int] = at((acc, _i) => acc + 1)
}

