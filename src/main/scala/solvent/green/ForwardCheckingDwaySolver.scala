package solvent.green

trait ForwardCheckingDwaySolver extends LoggingSolver with ForwardChecking {
  def solveAndLog(csp: CSP, log: (Solution, CSP) => Unit) = solve(Solution.empty, csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution, CSP) => Unit): Option[Solution] = {
    log(vars, csp)
    if (vars.size < csp.vars.size && !csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
      csp.selectAllNext(vars).map { case next @ (i, x) =>
        solve(vars + next, forwardCheck(i, csp.assign(i, x)), log)
      }.find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardCheckingDwaySolver extends ForwardCheckingDwaySolver
