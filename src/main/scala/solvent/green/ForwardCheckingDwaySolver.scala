package solvent.green

trait ForwardCheckingDwaySolver extends Solver with ForwardChecking {
  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (validIntermediate(vars, csp)) {
      csp.selectAllNext(vars).map { case next @ (i, x) =>
        solve(vars + next, forwardCheck(i, csp.assign(i, x)))
      }.find(_.isDefined).getOrElse(None)
    }
    else if (valid(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardCheckingDwaySolver extends ForwardCheckingDwaySolver with Logging
