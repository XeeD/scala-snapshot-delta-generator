package xeed.snapshotdelta

sealed trait Delta[T]
final case class Identical[T](value: T) extends Delta[T]
final case class Different[T](left: T, right: T, valueDelta: ValueDiff[T]) extends Delta[T]


sealed trait ValueDiff[+T]
final case class StringDiff(newValue: String) extends ValueDiff[String]
final case class IntDiff(delta: Int) extends ValueDiff[Int]
final case class DoubleDiff(delta: Double) extends ValueDiff[Double]
final case class SetValueDiff[E](added: Set[E], removed: Set[E]) extends ValueDiff[Set[E]]
final case class MapValueDiff[K, V](added: Map[K, V], removed: Map[K, V], modified: Map[K, V]) extends ValueDiff[Map[K, V]]

trait DeltaCalc[T] {
  def apply(left: T, right: T): ValueDiff[T]
}

object DeltaCalc {
  def apply[T](implicit deltaCalc: DeltaCalc[T]) = deltaCalc

  implicit object stringDeltaCalc extends DeltaCalc[String] {
    def apply(a: String, b: String) = StringDiff(a)
  }

  implicit def setDeltaCalc[E] = new DeltaCalc[Set[E]] {
    def apply(left: Set[E], right: Set[E]): SetValueDiff[E] = SetValueDiff(right -- left, left -- right)
  }

  implicit object intDeltaCalc extends DeltaCalc[Int] {
    def apply(a: Int, b: Int) = IntDiff(b - a)
  }

  implicit object doubleDeltaCalc extends DeltaCalc[Double] {
    def apply(a: Double, b: Double) = DoubleDiff(b - a)
  }

  implicit def mapDeltaCalc[K, V] = new DeltaCalc[Map[K, V]] {
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

object Diff {
  def apply[T](left: T, right: T)(implicit deltaCalc: DeltaCalc[T]) =
    if (left == right) Identical(left)
    else Different(left, right, deltaCalc(left, right))
}

object Main extends App {
  println(Diff("yo", "ho"))
  println(Diff("yolo", "yolo"))
  println(Diff(Set("foo", "bar"), Set("foo", "dummy")))
  println(Diff(Map("foo" -> 42, "bar" -> 666, "baz" -> true), Map("baz" -> true, "foo" -> 18, "dummy" -> 88)))

  case class Event(id: Int, price: Double, name: String, tags: Set[String], metadata: Map[String, String])

  val old = Event(250, 5.5, "Ice Hockey Czech Republic", Set("ice hockey", "hockey championship"), Map("sbtechOldId" -> "123"))
  val updated = Event(250, 2.2, "Ice Hockey Czech Republic vs Russia", Set("ice hockey", "hockey championship", "promoted"), Map("sbtechOldId" -> "123", "myPlay" -> "available"))

  println(SimpleDelta(old, updated))
}
