package xeed.snapshotdelta2

import shapeless._
import Delta._
import ValueDeltaCalc._

case class GameScore(state: String, homeTeam: Int, awayTeam: Int)
case class GameScoreExtended(state: String, homeTeam: Int, awayTeam: Int, foo: Foo)

sealed trait Foo
case class Bar(a: String, b: Int) extends Foo
case class Baz(value: Int) extends Foo
case class Dummy(value: Int) extends Foo

case class Event(name: String, score: GameScore)

object Main extends App {
  val a = GameScoreExtended("playing", 1, 3, Bar("haha", 1))
  val b = GameScoreExtended("stopped", 2, 3, Baz(2))

  val gameScoreOld = GameScore("playing", 1, 2)
  val gameScoreNew = GameScore("playing", 2, 2)

  val eventOld = Event("Football", gameScoreOld)
  val eventNew = Event("Football", gameScoreNew)

//  println(Delta[Event].run(eventOld, eventNew))

//  println(ValueDeltaCalc[Event](eventNew, eventOld))
  println(
    ValueDeltaCalc[Event].calculate(eventNew, eventOld)
  )
  val genEvent = LabelledGeneric[Event].to(eventNew)
}
