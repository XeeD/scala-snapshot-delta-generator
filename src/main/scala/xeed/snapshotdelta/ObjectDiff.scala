package xeed.snapshotdelta

sealed trait ObjectDiff[A]
final case class IdenticalField[A](value: A) extends ObjectDiff[A]
final case class DifferentField[V](left: V, right: V, delta: ValueDiff) extends ObjectDiff[V]

object ObjectDiff {
  def apply[A](left: A, right: A, deltaCalc: ValueDeltaCalc[A]): ObjectDiff[A] =
    if (left == right) IdenticalField(left)
    else DifferentField(left, right, deltaCalc(left, right))
}
