package xeed.snapshotdelta2

import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, LabelledGeneric, Lazy }
import xeed.snapshotdelta2.Delta.createDelta

sealed trait ValueDiff
case object Identical extends ValueDiff
case object DifferentTypes extends ValueDiff
case class Replacement(newValue: Any) extends ValueDiff
final case class NestedDiff(nested: Map[String, Any]) extends ValueDiff
final case class StringDiff(newValue: String) extends ValueDiff
final case class IntDiff(newValue: Int, delta: Int) extends ValueDiff
final case class DoubleDiff(delta: Double) extends ValueDiff
final case class SetValueDiff[E](added: Set[E], removed: Set[E]) extends ValueDiff
final case class MapValueDiff[K, V](added: Map[K, V], removed: Map[K, V], modified: Map[K, V]) extends ValueDiff

trait ValueDeltaCalc[T] {
  def calculate(left: T, right: T): ValueDiff
}

object ValueDeltaCalc {
  def apply[T](implicit deltaCalc: ValueDeltaCalc[T]) = deltaCalc

  def pure[T](fn: (T, T) => ValueDiff): ValueDeltaCalc[T] = new ValueDeltaCalc[T] {
    override def calculate(left: T, right: T) = fn(left, right)
  }

  implicit lazy val cnilVC: ValueDeltaCalc[CNil] = pure((left: CNil, right: CNil) => Identical)

  def findReplacement(cons: Coproduct): Replacement = cons match {
    case Inl(h) => Replacement(h)
    case Inr(t) => findReplacement(t)
  }

  implicit def cconsDelta[H, T <: Coproduct](
    implicit
    headDelta: Lazy[Delta[H]],
    tailDelta: ValueDeltaCalc[T],
  ): ValueDeltaCalc[H :+: T] =
    pure((left: H :+: T, right: H :+: T) => {
      (left, right) match {
        case (Inl(hl), Inl(hr)) => NestedDiff(headDelta.value.run(hl, hr))
        case (Inr(tl), Inr(tr)) => tailDelta.calculate(tl, tr)
        case _ => findReplacement(right)
      }
    })

  implicit lazy val hnilDelta: Delta[HNil] = new Delta[HNil] {
    override def run(left: HNil, right: HNil) = Map()
  }

  implicit def hconsGenDelta[H, GH <: HList, T <: HList](
    implicit
    genH: LabelledGeneric.Aux[H, GH],
    nested: Lazy[Delta[GH]],
  ): ValueDeltaCalc[H] =
    pure((left: H, right: H) =>
      NestedDiff(nested.value.run(genH.to(left), genH.to(right)))
    )

  implicit def coproductDelta[A, R ](
    implicit
    gen: Generic.Aux[A, R],
    deltaCalc: Lazy[ValueDeltaCalc[R]]
  ): ValueDeltaCalc[A] =
    pure((left: A, right: A) => deltaCalc.value.calculate(gen.to(left), gen.to(right)))

  implicit lazy val stringDeltaCalc =
    pure((a: String, b: String) => if (a == b) Identical else StringDiff(b))

  implicit def setDeltaCalc[E] =
    pure((left: Set[E], right: Set[E]) => SetValueDiff(right -- left, left -- right))

  implicit lazy val intDeltaCalc =
    pure((a: Int, b: Int) => if (a == b) Identical else IntDiff(b, b - a))

  implicit lazy val doubleDeltaCalc =
    pure((a: Double, b: Double) => if (a == b) Identical else DoubleDiff(b - a))

  implicit def mapDeltaCalc[K, V] =
    pure((left: Map[K, V], right: Map[K, V]) => {
      val newKeys = right.keySet -- left.keySet
      val removedKeys = left.keySet -- right.keySet
      val sameKeys = left.keySet -- removedKeys

      MapValueDiff[K, V](
        newKeys.map(k => k -> right(k)).toMap,
        removedKeys.map(k => k -> left(k)).toMap,
        sameKeys.filter(k => left(k) != right(k)).map(k => k -> right(k)).toMap
      )
    })

}

