package solvent.green

trait ForwardCheckingDwaySolver extends Solver with ForwardChecking {
  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (vars.size < csp.vars.size && !csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
      csp.selectAllNext(vars).map { case next @ (i, x) =>
        solve(vars + next, forwardCheck(i, csp.assign(i, x)))
      }.find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardCheckingDwaySolver extends ForwardCheckingDwaySolver with Logging
