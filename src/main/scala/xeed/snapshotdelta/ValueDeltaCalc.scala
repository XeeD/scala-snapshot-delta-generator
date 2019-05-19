package xeed.snapshotdelta

sealed trait ValueDiff[+T]
final case class StringDiff(newValue: String) extends ValueDiff[String]
final case class IntDiff(delta: Int) extends ValueDiff[Int]
final case class DoubleDiff(delta: Double) extends ValueDiff[Double]
final case class SetValueDiff[E](added: Set[E], removed: Set[E]) extends ValueDiff[Set[E]]
final case class MapValueDiff[K, V](added: Map[K, V], removed: Map[K, V], modified: Map[K, V]) extends ValueDiff[Map[K, V]]

trait ValueDeltaCalc[T] {
  def apply(left: T, right: T): ValueDiff[T]
}

object ValueDeltaCalc {
  def apply[T](implicit deltaCalc: ValueDeltaCalc[T]) = deltaCalc

  implicit object stringDeltaCalc extends ValueDeltaCalc[String] {
    def apply(a: String, b: String) = StringDiff(a)
  }

  implicit def setDeltaCalc[E] = new ValueDeltaCalc[Set[E]] {
    def apply(left: Set[E], right: Set[E]): SetValueDiff[E] = SetValueDiff(right -- left, left -- right)
  }

  implicit object intDeltaCalc extends ValueDeltaCalc[Int] {
    def apply(a: Int, b: Int) = IntDiff(b - a)
  }

  implicit object doubleDeltaCalc extends ValueDeltaCalc[Double] {
    def apply(a: Double, b: Double) = DoubleDiff(b - a)
  }

  implicit def mapDeltaCalc[K, V] = new ValueDeltaCalc[Map[K, V]] {
    def apply(left: Map[K, V], right: Map[K, V]): MapValueDiff[K, V] = {
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

