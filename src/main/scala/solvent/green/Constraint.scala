package solvent.green

/**
 * Defines allowed values for a given variable.
 *
 * @param varNum Index of the variable this constraint originates from.
 * @param constrainedVars Indices of the variables constrained by this variable.
 * @param allowedValues Allowed values of the constrained variables depending on the value of the source variable.
 */
case class Constraint(
  val varNum: Int,
  val constrainedVars: Seq[Int],
  val allowedValues: IndexedSeq[Iterable[(Int, Seq[Int])]]
) {
  /**
   * Check allowed variable values given a value for the variable this constraint originates from.
   *
   * @param value Value for this variable.
   * @return A number of tuples mapping from variable number to a sequence of allowed values.
   * @see varNum
   */
  def allowedFor(value: Int): Iterable[(Int, Seq[Int])] = allowedValues(value)
}
