package solvent.green

trait ForwardChecking2waySolver extends LoggingSolver with ForwardChecking {
  def solveAndLog(csp: CSP, log: (Solution, CSP) => Unit) = solve(Solution.empty, csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution, CSP) => Unit): Option[Solution] = {
    log(vars, csp)
    if (vars.size < csp.vars.size && !csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
      val next @ (i, x) = csp.selectNext(vars)
      solve(vars + next, forwardCheck(i, csp.assign(i, x)), log).orElse( // <left | x == n>
        solve(vars, forwardCheck(i, csp.prune(i, x)), log)) // <right | x != n>
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardChecking2waySolver extends ForwardChecking2waySolver
