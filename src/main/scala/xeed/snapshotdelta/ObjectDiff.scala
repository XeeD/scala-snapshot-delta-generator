package xeed.snapshotdelta

import shapeless.{ ::, DepFn2, Generic, HList, HNil, Poly2 }

sealed trait ObjectDiff[A]
final case class IdenticalField[A](value: A) extends ObjectDiff[A]
final case class DifferentField[V, VD](left: V, right: V, delta: ValueDiff[VD]) extends ObjectDiff[V]

object ObjectDiff {
  def apply[A](left: A, right: A, deltaCalc: DeltaCalc[A]): ObjectDiff[A] =
    if (left == right) IdenticalField(left)
    else DifferentField(left, right, deltaCalc(left, right))
}

trait SimpleDelta[R <: HList] extends DepFn2[R, R] {
  type Out <: HList
}

object SimpleDelta {
  type Aux[I <: HList, O <: HList] = SimpleDelta[I] { type Out = O }

  implicit def hnilDelta: Aux[HNil, HNil] =
    new SimpleDelta[HNil] {
      type Out = HNil

      def apply(t: HNil, u: HNil): Out = HNil
    }

  implicit def hconsDelta[H, T <: HList, DT <: HList]
    (implicit tailDelta: Aux[T, DT], deltaCalc: DeltaCalc[H]):
    Aux[H :: T, ObjectDiff[H] :: DT] =

    new SimpleDelta[H :: T] {
      type Out = ObjectDiff[H] :: DT

      def apply(left: H :: T, right: H :: T): Out =
        ObjectDiff(left.head, right.head, deltaCalc) :: tailDelta(left.tail, right.tail)
    }

  def apply[A, R <: HList](left: A, right: A)(implicit gen: Generic.Aux[A, R], delta: SimpleDelta[R]) =
    delta(gen.to(left), gen.to(right))
}
