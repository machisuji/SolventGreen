package solvent.green

trait BacktrackingDwaySolver extends LoggingSolver {

  def solveAndLog(csp: CSP, log: (Solution, CSP) => Unit) = solve(Solution.empty, csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution, CSP) => Unit): Option[Solution] = {
    log(vars, csp)
    if (vars.size < csp.vars.size && checkConstraints(vars, csp)) {
      csp.selectAllNext(vars).map(ix =>
        solve(vars + ix, csp, log)).find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object BacktrackingDwaySolver extends BacktrackingDwaySolver
