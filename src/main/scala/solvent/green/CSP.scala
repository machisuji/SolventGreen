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
    val domains = Map[Int, Domain]((for {
      i <- 0 until numVars
      line = lines.next
    } yield
      if (line startsWith "Set(") {
        val values = line.replace("Set(", "").replace(")", "").trim.split(",\\s*").map(_.toInt)
        i -> FineDomain(values)
      } else {
        val Array(start, end) = line.split(",\\s*").map(_.toInt)
        i -> Domain(start, end)
      }
    ): _*)

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

    CSP(domains.toIterable.toSeq.sortBy(_._1).map(_._2).toIndexedSeq, constraints.toSeq)
  }

  def fromFile(fileName: String) = fromSource(io.Source.fromFile(fileName))
}

/**
 * A Constraint Satisfaction Problem (CSP).
 *
 * @param domains Variable domains. Variables are indexed starting with 0.
 * @param constraints Constraints for variables.
 */
case class CSP(
  domains: IndexedSeq[Domain],
  constraints: Seq[Constraint],
  varOrder: VariableAssignmentOrder = DefaultVarOrder
) {
  val vars: Seq[Int] = 0 until domains.size

  def selectNext(sol: Solution) = {
    val i = varOrder.next(sol, this)
    i -> domains(i).head
  }

  def selectAllNext(sol: Solution) = {
    val i = varOrder.next(sol, this)
    domains(i).toIterator.map(value => i -> value)
  }

  def staticVarOrder(xs: Seq[Int]) = this.copy(varOrder = StaticVarOrder(xs))
  def reverseVarOrder = this.copy(varOrder = ReverseVarOrder)
  def smallestDomainVarOrder = this.copy(varOrder = SmallestDomainVarOrder)

  def prune(varNum: Int, value: Int) =
    CSP(domains.patch(varNum, IndexedSeq(domains(varNum) - value), 1), constraints, varOrder)

  def assign(varNum: Int, value: Int) =
    CSP(domains.patch(varNum, IndexedSeq(FineDomain(Seq(value))), 1), constraints, varOrder)
}

trait VariableAssignmentOrder {
  /**
   * Picks the next to-be-assigned variable depending on the given intermediate solution.
   *
   * @param sol The solution containing the currently assigned variables.
   * @param csp The problem to be solved.
   * @return The number (index) of the next variable (domain) to get values for (from).
   */
  def next(sol: Solution, csp: CSP): Int
}

case object DefaultVarOrder extends VariableAssignmentOrder {
  def next(sol: Solution, csp: CSP): Int = csp.vars(sol.size)
}

case class StaticVarOrder(xs: Seq[Int]) extends VariableAssignmentOrder {
  def next(sol: Solution, csp: CSP): Int = xs(sol.size)
}

case object ReverseVarOrder extends VariableAssignmentOrder {
  def next(sol: Solution, csp: CSP): Int = csp.vars.reverse(sol.size)
}

case object SmallestDomainVarOrder extends VariableAssignmentOrder {
  def next(sol: Solution, csp: CSP): Int =
    csp.vars.zip(csp.domains).filterNot {
      case (i, domain) => sol.contains(i)
    }.sortBy {
      case (i, domain) => domain.size
    }.headOption.map(_._1).getOrElse {
      throw new IllegalStateException("Cannot pick next variable. All variables are already assigned.")
    }
}
