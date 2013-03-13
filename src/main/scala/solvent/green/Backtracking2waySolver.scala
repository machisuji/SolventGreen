package solvent.green

trait Backtracking2waySolver extends Solver {
  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (validIntermediate(vars, csp)) { // incomplete solution ok so far
      val next @ (i, x) = csp.selectNext(vars) // i = var, x = value
      solve(vars + next, left(i, x, csp)) orElse solve(vars, right(i, x, csp)) // branch
    }
    else if (valid(vars, csp)) Some(vars) // solution complete and valid
    else None // no solution found
  }

  /**
   * Changes a CSP for left branching, i.e. assigns the value of the current variable.
   *
   * @param i current variable (number/index)
   * @param x variable's value
   * @param csp problem to be solved
   * @return A new CSP whose domain for the current variable is pruned to the assigned value.
   */
  def left(i: Int, x: Int, csp: CSP) = csp.assign(i, x)

  /**
   * Changes a CSP for right branching, i.e. prunes the value of the current variable from its domain.
   *
   * @param i current variable (number/index)
   * @param x variable's value
   * @param csp problem to be solved
   * @return A new CSP whose domain for the current variable is pruned by the current value.
   */
  def right(i: Int, x: Int, csp: CSP) = csp.prune(i, x)
}

case object Backtracking2waySolver extends Backtracking2waySolver with Logging
