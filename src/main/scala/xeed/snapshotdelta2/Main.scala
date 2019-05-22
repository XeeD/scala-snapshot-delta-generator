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
case class Dummy(value: Int) extends Foo

case class Event(name: String, score: GameScore)

object Main extends App {
  val a = GameScoreExtended("playing", 1, 3, Bar("haha", 1))
  val b = GameScoreExtended("stopped", 2, 3, Baz(2))

  val eventOld = Event("Football", GameScore("playing", 1, 2))
  val eventNew = Event("Football", GameScore("playing", 2, 3))

  println(Delta[GameScoreExtended].run(a, b))
//  println(Delta[Event].run(eventOld, eventNew))
}
