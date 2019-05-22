package xeed.snapshotdelta2

import shapeless._
import Delta._
import ValueDeltaCalc._

case class GameScore(state: String, homeTeam: Int, awayTeam: Int, dummy: DummyN)
case class GameScoreExtended(state: State, homeTeam: Int, awayTeam: Int, foo: Foo)

case class DummyN(a: String, b: Int)

sealed trait Foo
case class Bar(a: String, b: Int) extends Foo
case class Baz(value: Int) extends Foo
case class Dummy(value: Int) extends Foo

sealed trait State
case object Playing extends State
case object Stopped extends State

case class Event(name: String, score: GameScore, scoreExtended: GameScoreExtended)

object Main extends App {
  val a = GameScoreExtended(Playing, 1, 3, Bar("haha", 1))
  val b = GameScoreExtended(Stopped, 2, 3, Baz(2))

  val gameScoreOld = GameScore("playing", 1, 2, DummyN("a", 1))
  val gameScoreNew = GameScore("playing", 2, 3, DummyN("b", 1))

  val eventOld = Event("Football", gameScoreOld, a)
  val eventNew = Event("Football", gameScoreNew, b)

  println(
    ValueDeltaCalc[GameScoreExtended].calculate(a, b)
  )

  println(
    ValueDeltaCalc[Event].calculate(eventOld, eventNew)
  )
}
