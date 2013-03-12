package solvent.green

object CSP {
  /**
   * Reads CSPs in the form of that which can be found under 'src/main/resources/csp/4Queens.csp'.
   *
   * The original format is enhanced in two ways.
   * Firstly a variable assignment order can be specified following the number of variables.
   * This can be an explicit static order like this:
   *     // number of variables:
   *     4 (0, 1, 3, 2)
   * It can also be a dynamic order, specifically smallest domain first:
   *     // number of variables:
   *     4 (smallest domain)
   *
   * Moreover not only ranges but also sets can be given as domains:
   *     // Domains of the variables:
   *     Set(2, 10, 16)
   *
   * @param source Source of CSP file to be loaded.
   * @return A CSP instance with constraints according to the loaded file.
   */
  def fromSource(source: io.Source) = {
    val lines = source.getLines.map(_.trim).filterNot(line =>
      line.startsWith("//") || line.isEmpty)
    val Some((numVars, varOrder)) = Some(lines.next).map { line =>
      if (line.contains(",")) { // static order
        line.split("\\(").head.trim.toInt ->
          StaticVarOrder(line.dropWhile(c => c != '(').tail.init.split(",\\s*").map(_.toInt))
      } else if (line.toLowerCase.contains("smallest") && line.toLowerCase.contains("domain")) {
        line.split("\\(").head.trim.toInt -> SmallestDomainVarOrder
      } else {
        line.toInt -> DefaultVarOrder
      }
    }
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

    CSP(domains.toIterable.toSeq.sortBy(_._1).map(_._2).toIndexedSeq, constraints.toSeq, varOrder)
  }

  def fromFile(fileName: String) = fromSource(io.Source.fromFile(fileName))
}

/**
 * A Constraint Satisfaction Problem (CSP).
 *
 * @param domains Variable domains. Variables are indexed starting with 0.
 * @param constraints Constraints for variables.
 * @param varOrder Variable assignment order
 */
case class CSP(
  domains: IndexedSeq[Domain],
  constraints: Seq[Constraint],
  varOrder: VariableAssignmentOrder = DefaultVarOrder
) {
  val vars: Seq[Int] = 0 until domains.size

  /**
   * Selects the next variable value according to this CSP's variable assignment order.
   *
   * @param sol The intermediate solution with the already assigned variables.
   * @return A tuple of variable index and value.
   */
  def selectNext(sol: Solution) = {
    val i = varOrder.next(sol, this)
    i -> domains(i).head
  }

  /**
   * Selects all possible next variable values according to this CSP's variable assignment order.
   *
   * @param sol The intermediate solution with the already assigned variables.
   * @return An iterator of tuples of variable index and value.
   */
  def selectAllNext(sol: Solution) = {
    val i = varOrder.next(sol, this)
    domains(i).toIterator.map(value => i -> value)
  }

  def staticVarOrder(xs: Seq[Int]) = this.copy(varOrder = StaticVarOrder(xs))
  def reverseVarOrder = this.copy(varOrder = ReverseVarOrder)
  def smallestDomainVarOrder = this.copy(varOrder = SmallestDomainVarOrder)

  /**
   * Prunes a domain by a single value.
   *
   * @param varNum The index of the domain to be pruned.
   * @param value The value to be removed from the domain.
   * @return A new CSP with the pruned domain.
   */
  def prune(varNum: Int, value: Int) =
    CSP(domains.patch(varNum, IndexedSeq(domains(varNum) - value), 1), constraints, varOrder)

  /**
   * Assigns a variable, i.e. removes all other values except the assigned one from the respective domain.
   *
   * @param varNum The index of the variable to be assigned.
   * @param value The value to be assigned to the variable.
   * @return A new CSP with the assigned the assigned variable's pruned domain.
   */
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
      case (i, domain) => sol.contains(i) // only consider not-yet-assigned variables
    }.sortBy {
      case (i, domain) => domain.size // select smallest domains first
    }.headOption.map(_._1).getOrElse {
      throw new IllegalStateException("Cannot pick next variable. All variables are already assigned.")
    }
}
