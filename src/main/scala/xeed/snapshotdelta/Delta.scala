package xeed.snapshotdelta

import shapeless.{ ::, DepFn2, Generic, HList, HNil }

trait Delta[R <: HList] extends DepFn2[R, R] {
  type Out <: HList
}

object Delta extends LowPriorityDelta {
  type Aux[I <: HList, O <: HList] = Delta[I] { type Out = O }

  implicit def hnilDelta: Aux[HNil, HNil] =
    new Delta[HNil] {
      type Out = HNil

      def apply(t: HNil, u: HNil): Out = HNil
    }

  implicit def hconsGenDelta[H, GH <: HList, DH <: HList, T <: HList, DT <: HList]
  (implicit
    genH: Generic.Aux[H, GH],
    nested: Delta.Aux[GH, DH],
    tailDelta: Delta.Aux[T, DT]):
  Aux[H :: T, DH :: DT] =

    new Delta[H :: T] {
      type Out = DH :: DT

      def apply(left: H :: T, right: H :: T): Out =
        nested(genH.to(left.head), genH.to(right.head)) :: tailDelta(left.tail, right.tail)
    }

  def apply[A, R <: HList]
  (left: A, right: A)
    (implicit gen: Generic.Aux[A, R], delta: Delta[R]) =
    delta(gen.to(left), gen.to(right))
}

trait LowPriorityDelta {
  implicit def hconsDelta[H, T <: HList, DT <: HList]
  (implicit tailDelta: Delta.Aux[T, DT], deltaCalc: ValueDeltaCalc[H]):
  Delta.Aux[H :: T, ObjectDiff[H] :: DT] =

    new Delta[H :: T] {
      type Out = ObjectDiff[H] :: DT

      def apply(left: H :: T, right: H :: T): Out =
        ObjectDiff(left.head, right.head, deltaCalc) :: tailDelta(left.tail, right.tail)
    }
}
