package solvent.green

/**
 * A CSP (Constraint Satisfaction Problem) Solver
 */
trait Solver {

  type Solution = IndexedSeq[Int]

  /**
   * Tries to solve a CSP.
   *
   * @param csp The problem to be solved.
   * @return Some(Solution) if a valid solution could be found or None if no solution could be found.
   */
  def solve(csp: CSP): Option[Solution]

  /**
   * Validates that a solution is applicable to a given problem and that it satisfies all constraints.
   * Note to myself: checking for domains is unnecessary since the constraints are extensional anyway,
   * but perhaps there will be intensional ones one day.
   *
   * @param solution The candidate solution to be validated.
   * @param csp The problem to the given solution.
   * @return True if the solution is valid, false otherwise.
   */
  def validate(solution: Solution, csp: CSP) =
    solution.size == csp.vars.size &&
    solution.zip(csp.domains).forall { case (value, domain) => domain contains value } &&
    checkConstraints(solution, csp)

  /**
   * Checks all applicable constraints for an intermediate solution.
   * That is the solution does not have to be complete.
   * A check of an empty solution will return true since it does not violate any constraints, yet.
   * A solution has to be validated before it can be decided on.
   *
   * @param solution The (intermediate) solution to be checked against applicable constraints.
   * @param csp The problem to be checked against.
   * @return True if the intermediate solution does not violate any applicable constraints, false otherwise.
   * @see #validate(Solution, CSP)
   */
  def checkConstraints(solution: Solution, csp: CSP) =
    csp.constraints.filter(con =>
      (con.varNum +: con.constrainedVars).forall(solution.size >)).forall { con =>
        val allowed = con.allowedFor(solution(con.varNum))
        allowed.nonEmpty && allowed.forall{ case (col, values) =>
          values contains solution(col) }
      }
}

/**
 * A CSP Solver that provides the ability to log every intermediate solution (search node).
 */
trait LoggingSolver extends Solver {
  /**
   * Tries to solve a given CSP while reporting every intermediate solution on the search path
   * through a given log function.
   *
   * @param csp The problem to be solved.
   * @param log Log function called with every intermediate solution.
   * @return Some(Solution) if a valid solution could be found or None if no solution could be found.
   */
  def solveAndLog(csp: CSP, log: (Solution) => Unit): Option[Solution]

  def solve(csp: CSP) = solveAndLog(csp, _ => ())

  /**
   * Tries to solve the given CSP while also logging the number of explored search nodes
   * and measuring the overall time it took.
   *
   * @param csp The problem to be solved.
   * @param show If true each intermediate solution (variable assignment) will be printed to stdout.
   * @return A 3-tuple with the (possible) result, the number of explored search nodes and the time in nanoseconds.
   */
  def solutionAndInfo(csp: CSP, show: Boolean = false) = {
    var calls: Int = 0
    def log(sol: Solution) {
      if (show) {
        println(sol)
      }
      calls += 1
    }
    val (result, ns) = time(solveAndLog(csp, log))
    (result, calls, ns)
  }

  /**
   * Executes a given action while measuring the required time.
   *
   * @param action Action to be performed.
   * @tparam R Type of the expected result.
   * @return A 2-tuple consisting of the result of the action as well as the measured time in nanoseconds.
   */
  def time[R](action: => R): (R, Long) = {
    val ns = System.nanoTime
    val result = action

    (result, System.nanoTime - ns)
  }
}
