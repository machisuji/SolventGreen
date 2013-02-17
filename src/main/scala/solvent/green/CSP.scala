package solvent.green

object CSP {
  /**
   * Reads CSPs in the form of that which can be found under 'src/main/resources/csp/4Queens.csp'.
   *
   * @param source Source of CSP file to be loaded.
   * @return A CSP instance with constraints according to the loaded file.
   */
  def fromSource(source: io.Source) = {
    val lines = source.getLines.map(_.trim).filterNot(line =>
      line.startsWith("//") || line.isEmpty)
    val numVars = lines.next.toInt
    val domains = Map[Int, Range.Inclusive]((for {
      i <- 0 until numVars
      Array(start, end) = lines.next.split(",\\s*").map(_.toInt)
    } yield i -> (start to end)): _*)

    var c: Option[String] = None
    val constraints = lines.map { line =>
      val vars = "\\d+".r.findAllIn(c.getOrElse(line)).toList.map(_.toInt).toIndexedSeq
      def notNextConstraint(line: String) = if (line.startsWith("c")) { c = Some(line); false } else true
      val prefix = c.map(_ => line).toIterator // line may be not "c(0, 1)" but already values such as "0, 3"
      c = None

      val values = (prefix ++ lines).takeWhile(notNextConstraint).map(line =>
        "\\d+".r.findAllIn(line).map(_.toInt).toSeq).toSeq.groupBy(_.head).map {
          case (key, value) => (key, value.map(_.zipWithIndex.tail).flatten)
        }.map { case (value: Int, allowedByColumn: Seq[(Int, Int)]) =>
          val allowed = allowedByColumn.groupBy(_._2).map { case (col: Int, tuples: Seq[(Int, Int)]) =>
            vars(col) -> tuples.map(_._1).toList
          }
          value -> allowed.toSeq
        }

      Constraint(vars.head, vars.tail, values)
    }

    CSP(domains.toIterable.toSeq.sortBy(_._1).map(_._2).map(range =>
      Domain(range.start, range.end)).toIndexedSeq, constraints.toSeq)
  }

  def fromFile(fileName: String) = fromSource(io.Source.fromFile(fileName))
}

/**
 * A Constraint Satisfaction Problem (CSP).
 *
 * @param domains Variable domains. Variables are indexed starting with 0.
 * @param constraints Constraints for variables.
 */
case class CSP(domains: IndexedSeq[Domain], constraints: Seq[Constraint]) {
  val vars: Seq[Int] = 0 until domains.size
}
