package solvent.green

/**
 * Defines allowed values for a given variable, i.e. an extensional sub-domain.
 *
 * @param varNum Index of the variable this constraint originates from.
 * @param constrainedVars Indices of the variables constrained by this variable.
 * @param allowedValueMap Allowed values of the constrained variables depending on the value of the source variable.
 */
case class Constraint(
  val varNum: Int,
  val constrainedVars: Seq[Int],
  val allowedValueMap: Map[Int, Iterable[(Int, Seq[Int])]]
) {
  /**
   * Check allowed variable values given a value for the variable this constraint originates from.
   *
   * @param value Value for this variable.
   * @return A number of tuples mapping from variable number to a sequence of allowed values.
   * @see varNum
   */
  def allowedFor(value: Int): Iterable[(Int, Seq[Int])] = allowedValueMap.getOrElse(value, Iterable())
  def domainsFor(value: Int): Iterable[(Int, Domain)] = allowedFor(value).map(e => e._1 -> Domain(e._2))

  def values = allowedValueMap.keys

  override def toString = {
    def displayAllowed = allowedValueMap.map { case (value, allowed: Iterable[(Int, Seq[Int])]) =>
      def displayValues(entry: (Int, Seq[Int])) =
        s"${entry._1} -> [${entry._2.mkString(", ")}]"
      s"$value => {${allowed.map(displayValues).mkString(", ")}}"
    }
    s"Constraint($varNum -> [${constrainedVars.mkString(", ")}], allowedFor: ${displayAllowed.mkString(", ")}})"
  }
}
