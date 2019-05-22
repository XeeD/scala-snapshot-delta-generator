package xeed.snapshotdelta2

import shapeless._
import shapeless.labelled.FieldType

trait Delta[A] {
  def run(left: A, right: A): Map[String, Any]
}

object Delta {
  def apply[A](implicit deltaA: Delta[A]): Delta[A] = deltaA

  def createDelta[A](fn: (A, A) => Map[String, Any]): Delta[A] = new Delta[A] {
    override def run(left: A, right: A) = fn(left, right)
  }

  implicit lazy val hnilDelta: Delta[HNil] = createDelta((_, _) => Map())

  implicit def hconsDelta[H, K <: Symbol, T <: HList](
    implicit
    witness: Witness.Aux[K],
    deltaCalc: ValueDeltaCalc[H],
    tailDelta: Delta[T]
  ): Delta[FieldType[K, H] :: T] =
    createDelta[FieldType[K, H] :: T](
      (left, right) =>
        Map(witness.value.name -> deltaCalc(left.head, right.head))
          ++ tailDelta.run(left.tail, right.tail)
    )


  implicit def genericDelta[A, R <: HList](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    delta: Delta[R]
  ): Delta[A] =
    createDelta[A]((left, right) => delta.run(gen.to(left), gen.to(right)))
}
