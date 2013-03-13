package solvent.green

trait Backtracking2waySolver extends Solver {
  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (validIntermediate(vars, csp)) {
      val next @ (i, x) = csp.selectNext(vars)
      solve(vars + next, csp.assign(i, x)) orElse solve(vars, csp.prune(i, x)) // left orElse right
    }
    else if (valid(vars, csp)) Some(vars)
    else None
  }
}

case object Backtracking2waySolver extends Backtracking2waySolver with Logging
