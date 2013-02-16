package solvent.green

/**
 * A variable's domain.
 */
trait Domain extends Iterable[Int] {
  def lowerBound: Int
  def upperBound: Int

  def contains(value: Int): Boolean
}

case class LargeDomain(lowerBound: Int, upperBound: Int) extends Domain {

  val ranges: Seq[Range.Inclusive] = Seq(lowerBound to upperBound)

  class Iterator extends collection.Iterator[Int] {
    var outerIndex = 0
    var innerIndex = 0

    def hasNext = withinInner || withinOuter
    def next() = {
      if (!withinInner) {
        innerIndex = 0
        outerIndex += 1
      }
      if (withinOuter) currentRange(innerIndex)
      else throw new NoSuchElementException("There's no next element available.")
    }

    def currentRange = ranges(outerIndex)
    def withinInner = ranges(outerIndex).end > ranges(outerIndex)(innerIndex)
    def withinOuter = outerIndex < ranges.size
  }

  def contains(value: Int) = ranges.exists(_ contains value)
  def iterator = new Iterator

  override def toString = "LargeDomain(" + ranges.map(r => s"[${r.start}..${r.end}]").mkString(", ") + ")"
}

case class SmallDomain(lowerBound: Int, upperBound: Int) extends Domain {
  val values = (lowerBound to upperBound).toSeq

  def contains(value: Int) = values contains value
  def iterator = values.toIterator
}

object Domain {
  def apply(lb: Int, ub: Int): Domain =
    if (math.abs(ub - lb) >= 10) LargeDomain(lb, ub)
    else SmallDomain(lb, ub)
}
