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

  def contains(value: Int) = ranges.exists(_ contains value)
  def iterator = ranges.map(_.toIterator).reduce(_ ++ _)

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
