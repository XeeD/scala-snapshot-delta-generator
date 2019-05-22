package xeed.snapshotdelta2

case class Venue(name: String, location: String)

object Main extends App {
  val a = Venue("bla", "goo")
  val b = Venue("bla", "foo")

  println(Delta[Venue].run(a, b))
}
