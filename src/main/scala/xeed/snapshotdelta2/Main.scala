package xeed.snapshotdelta2

import shapeless._
import Delta._
import ValueDeltaCalc._

case class GameScore(state: String, homeTeam: Int, awayTeam: Int)
case class GameScoreExtended(state: String, homeTeam: Int, awayTeam: Int, foo: Foo)

sealed trait Foo
//case object Foo1 extends Foo
case class Bar(a: String, b: Int) extends Foo
case class Baz(value: Int) extends Foo

object Main extends App {
  val a = GameScoreExtended("playing", 1, 3, Bar("haha", 1))
  val b = GameScoreExtended("stopped", 2, 3, Bar("haha", 2))

  implicitly[ValueDeltaCalc[Bar :+: CNil]]
  implicitly[ValueDeltaCalc[Foo]]
  println(Delta[GameScoreExtended].run(a, b))
}
