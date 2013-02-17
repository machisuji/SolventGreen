package solvent.green

/**
 * A variable's domain.
 */
trait Domain extends Iterable[Int] {
  def lowerBound: Int
  def upperBound: Int

  def contains(value: Int): Boolean
}

case class CoarseDomain(lowerBound: Int, upperBound: Int) extends Domain {
  val ranges: Seq[Range.Inclusive] = Seq(lowerBound to upperBound)

  def contains(value: Int) = ranges.exists(_ contains value)
  def iterator = ranges.map(_.toIterator).reduce(_ ++ _)

  override def toString = "c.Domain(" + ranges.map(r => s"[${r.start}..${r.end}]").mkString(", ") + ")"
}

case class FineDomain(values: Seq[Int]) extends Domain {
  def lowerBound = values.head
  def upperBound = values.last

  def contains(value: Int) = values contains value
  def iterator = values.toIterator

  override def toString = s"f.Domain(${values.mkString(",")})"
  override def toSeq = values
}

object Domain {
  def apply(lb: Int, ub: Int): Domain =
    if (math.abs(ub - lb) >= 10) CoarseDomain(lb, ub)
    else FineDomain((lb to ub).toSeq)

  def apply(values: Seq[Int]): Domain = FineDomain(values)
}
