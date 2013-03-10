package solvent.green

import language.postfixOps

/**
 * A variable's domain.
 */
trait Domain extends Iterable[Int] {
  def lowerBound: Int
  def upperBound: Int

  def contains(value: Int): Boolean
  def prune(p: (Int) => Boolean): Domain
  def pruneNot(p: (Int) => Boolean): Domain

  /**
   * Remove an element from the domain.
   *
   * @param x The element to be removed.
   * @return A new Domain object with the pruned set of values.
   */
  def -(x: Int): Domain
}

case class CoarseDomain(ranges: Seq[Seq[Int]]) extends Domain {
  def lowerBound = ranges.head.head
  def upperBound = ranges.last.last

  def contains(value: Int) = ranges.exists(_ contains value)
  def iterator = ranges.map(_.toIterator).reduceOption(_ ++ _) getOrElse Iterator.empty

  def -(x: Int): Domain = {
    val newRanges = ranges.flatMap(range =>
      if (range contains x) Seq(range.head to (x - 1), (x + 1) to range.last).filterNot(_.isEmpty)
      else Seq(range))
    CoarseDomain(newRanges)
  }

  def prune(p: (Int) => Boolean): Domain = CoarseDomain(ranges.map { range =>
      val filtered = range filterNot p
      if (filtered == range) range else filtered
    })

  def pruneNot(p: (Int) => Boolean): Domain = CoarseDomain(ranges.map { range =>
    val filtered = range filter p
    if (filtered == range) range else filtered
  })

  override def toString = "c.Domain(" + ranges.map(r =>
    if (r.nonEmpty) s"[${r.head}..${r.last}]" else "[]").mkString(", ") + ")"
}

object CoarseDomain {
  def apply(lb: Int, ub: Int): CoarseDomain = CoarseDomain(Seq(lb to ub))
}

case class FineDomain(values: Seq[Int]) extends Domain {
  def lowerBound = values.head
  def upperBound = values.last

  def contains(value: Int) = values contains value
  def iterator = values.toIterator

  def -(x: Int): Domain = FineDomain(values.filter(x !=))

  def prune(p: (Int) => Boolean): Domain = FineDomain(values filterNot p)
  def pruneNot(p: (Int) => Boolean): Domain = FineDomain(values filter p)

  override def toString = s"f.Domain(${values.mkString(",")})"
  override def toSeq = values
}

object Domain {
  def apply(lb: Int, ub: Int): Domain =
    if (math.abs(ub - lb) >= 10) CoarseDomain(lb, ub)
    else FineDomain((lb to ub).toSeq)

  def apply(values: Seq[Int]): Domain = FineDomain(values)

  def empty = Domain(Seq())
}
