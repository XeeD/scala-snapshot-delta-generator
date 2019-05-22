package xeed.snapshotdelta

trait DeltaWithKeys[A] {
  def apply(left: A, right: A): ValueDiff
}

