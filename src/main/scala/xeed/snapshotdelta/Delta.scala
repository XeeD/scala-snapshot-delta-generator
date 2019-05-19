package xeed.snapshotdelta

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

object Main extends App {
//  println(Delta("yo", "ho"))
//  println(Delta("yolo", "yolo"))
//  println(Delta(Set("foo", "bar"), Set("foo", "dummy")))
//  println(Delta(Map("foo" -> 42, "bar" -> 666, "baz" -> true), Map("baz" -> true, "foo" -> 18, "dummy" -> 88)))

  case class GameScore(homeTeam: Int, awayTeam: Int)
  case class Venue(name: String, location: String)
  case class Event(
    id: Int, price: Double, name: String, tags: Set[String], metadata: Map[String, String],
//    gameScore: GameScore,
//    venue: Venue
  )

  val oldScore = GameScore(1, 3)
  val newScore = GameScore(4, 3)

  val old = Event(
    250, 5.5, "Ice Hockey Czech Republic", Set("ice hockey", "hockey championship"), Map("sbtechOldId" -> "123"),
//    GameScore(1, 3),
//    Venue("Sazka Arena", "Prague, CZ")
  )
  val updated = Event(
    250, 2.2, "Ice Hockey Czech Republic vs Russia", Set("ice hockey", "hockey championship", "promoted"), Map("sbtechOldId" -> "123", "myPlay" -> "available"),
//    GameScore(4, 3),
//    Venue("Sazka Arena", "Prague, CZ")
  )

//  println(Delta(old, updated))
  println(Delta(oldScore, newScore))
}
