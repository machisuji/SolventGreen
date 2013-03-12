package solvent

package object green {
  type Solution = Map[Int, Int]

  object Solution {
    def apply(values: (Int, Int)*): Map[Int, Int] = Map(values: _*)
    def empty = Map[Int, Int]()
  }

  implicit class RichSolution(val solution: Solution) extends AnyVal {
    def toOrderedSeq: Seq[Int] = solution.toSeq.sortBy(_._1).map(_._2)
  }
}
