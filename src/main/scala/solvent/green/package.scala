package solvent

package object green {
  /**
   * A solution maps variable indices to values.
   */
  type Solution = Map[Int, Int]

  object Solution {
    def apply(values: (Int, Int)*): Map[Int, Int] = Map(values: _*)
    def empty = Map[Int, Int]()
  }

  implicit class RichSolution(val solution: Solution) extends AnyVal {
    /**
     * Gives a solution's values in order of their respective domains.
     *
     * @return A sequence of solution values.
     */
    def toOrderedSeq: Seq[Int] = solution.toSeq.sortBy(_._1).map(_._2)
  }
}
