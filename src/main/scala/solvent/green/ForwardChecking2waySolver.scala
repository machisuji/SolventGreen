package solvent.green

trait ForwardChecking2waySolver extends Solver with ForwardChecking {
  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (validIntermediate(vars, csp)) {
      val next @ (i, x) = csp.selectNext(vars)
      solve(vars + next, forwardCheck(i, csp.assign(i, x))).orElse( // <left | x == n>
        solve(vars, forwardCheck(i, csp.prune(i, x)))) // <right | x != n>
    }
    else if (valid(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardChecking2waySolver extends ForwardChecking2waySolver with Logging
