package xeed.snapshotdelta2

import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, LabelledGeneric, Lazy }
import xeed.snapshotdelta2.Delta.createDelta

sealed trait ValueDiff
case object Identical extends ValueDiff
case object DifferentTypes extends ValueDiff
case class Replacement(newValue: Any) extends ValueDiff
final case class NestedDiff(nested: Map[String, Any]) extends ValueDiff
final case class StringDiff(newValue: String) extends ValueDiff
final case class IntDiff(delta: Int) extends ValueDiff
final case class DoubleDiff(delta: Double) extends ValueDiff
final case class SetValueDiff[E](added: Set[E], removed: Set[E]) extends ValueDiff
final case class MapValueDiff[K, V](added: Map[K, V], removed: Map[K, V], modified: Map[K, V]) extends ValueDiff

trait ValueDeltaCalc[T] {
  def calculate(left: T, right: T): ValueDiff
}

object ValueDeltaCalc {
  def apply[T](implicit deltaCalc: ValueDeltaCalc[T]) = deltaCalc

  implicit lazy val cnilVC: ValueDeltaCalc[CNil] = new ValueDeltaCalc[CNil] {
    override def calculate(left: CNil, right: CNil) = Identical
  }
  implicit lazy val cnilDelta: Delta[CNil] = new Delta[CNil] {
    override def run(left: CNil, right: CNil) = ???
  }

  def findReplacement(cons: Coproduct): Replacement = cons match {
    case Inl(h) => Replacement(h)
    case Inr(t) => findReplacement(t)
  }

  implicit def cconsDelta[H, T <: Coproduct](
    implicit
    headDelta: Lazy[Delta[H]],
    tailDelta: ValueDeltaCalc[T],
  ): ValueDeltaCalc[H :+: T] = new ValueDeltaCalc[H :+: T] {
    override def calculate(left: H :+: T, right: H :+: T) = {
      (left, right) match {
        case (Inl(hl), Inl(hr)) => NestedDiff(headDelta.value.run(hl, hr))
        case (Inr(tl), Inr(tr)) => tailDelta.calculate(tl, tr)
        case _ => findReplacement(right)
      }
    }
  }

  implicit lazy val hnilDelta: Delta[HNil] = new Delta[HNil] {
    override def run(left: HNil, right: HNil) = Map()
  }

  implicit def hconsGenDelta[H, GH <: HList, T <: HList](
    implicit
    genH: LabelledGeneric.Aux[H, GH],
    nested: Lazy[Delta[GH]],
  ): ValueDeltaCalc[H] = new ValueDeltaCalc[H] {
    override def calculate(left: H, right: H) =
      NestedDiff(nested.value.run(genH.to(left), genH.to(right)))
  }

  implicit def coproductDelta[A, R ](
    implicit
    gen: Generic.Aux[A, R],
    deltaCalc: Lazy[ValueDeltaCalc[R]]
  ): ValueDeltaCalc[A] = new ValueDeltaCalc[A] {
    override def calculate(left: A, right: A): ValueDiff = deltaCalc.value.calculate(gen.to(left), gen.to(right))
  }

  implicit object stringDeltaCalc extends ValueDeltaCalc[String] {
    def calculate(a: String, b: String) = if (a == b) Identical else StringDiff(a)
  }

  implicit def setDeltaCalc[E] = new ValueDeltaCalc[Set[E]] {
    def calculate(left: Set[E], right: Set[E]): SetValueDiff[E] = SetValueDiff(right -- left, left -- right)
  }

  implicit object intDeltaCalc extends ValueDeltaCalc[Int] {
    def calculate(a: Int, b: Int) = if (a == b) Identical else IntDiff(b - a)
  }

  implicit object doubleDeltaCalc extends ValueDeltaCalc[Double] {
    def calculate(a: Double, b: Double) = if (a == b) Identical else DoubleDiff(b - a)
  }

  implicit def mapDeltaCalc[K, V] = new ValueDeltaCalc[Map[K, V]] {
    def calculate(left: Map[K, V], right: Map[K, V]): MapValueDiff[K, V] = {
      val newKeys = right.keySet -- left.keySet
      val removedKeys = left.keySet -- right.keySet
      val sameKeys = left.keySet -- removedKeys

      MapValueDiff[K, V](
        newKeys.map(k => k -> right(k)).toMap,
        removedKeys.map(k => k -> left(k)).toMap,
        sameKeys.filter(k => left(k) != right(k)).map(k => k -> right(k)).toMap
      )
    }
  }

}

