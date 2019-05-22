package xeed.snapshotdelta

object Main extends App {
//  println(Delta("yo", "ho"))
//  println(Delta("yolo", "yolo"))
//  println(Delta(Set("foo", "bar"), Set("foo", "dummy")))
//  println(Delta(Map("foo" -> 42, "bar" -> 666, "baz" -> true), Map("baz" -> true, "foo" -> 18, "dummy" -> 88)))

  case class GameScore(homeTeam: Int, awayTeam: Int)
  case class Venue(name: String, location: String)
  case class Event(
//    venue2: Venue,
    id: Int, price: Double, name: String, tags: Set[String], metadata: Map[String, String],
//    gameScore: GameScore,
//    venue: Venue
  )

  val oldScore = GameScore(1, 3)
  val newScore = GameScore(4, 3)

  val old = Event(
//    Venue("Sazka Arena", "Prague, CZ"),
    250, 5.5, "Ice Hockey Czech Republic", Set("ice hockey", "hockey championship"), Map("sbtechOldId" -> "123"),
//    GameScore(1, 3),
//    Venue("Sazka Arena", "Prague, CZ")
  )
  val updated = Event(
//    Venue("Sazka Arena", "Prague, CZ"),
    250, 2.2, "Ice Hockey Czech Republic vs Russia", Set("ice hockey", "hockey championship", "promoted"), Map("sbtechOldId" -> "123", "myPlay" -> "available"),
//    GameScore(4, 3),
//    Venue("Sazka Arena", "Prague, CZ")
  )

  println(Delta(old, updated).foldLeft(List[ValueDiff]())(deltaToMap))
  println(Delta(oldScore, newScore).foldLeft(List[ValueDiff]())(deltaToMap))
}
