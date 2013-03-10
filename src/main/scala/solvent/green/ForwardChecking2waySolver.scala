package solvent.green

trait ForwardChecking2waySolver extends LoggingSolver with ForwardChecking {
  def solveAndLog(csp: CSP, log: (Solution) => Unit) = solve(IndexedSeq(), csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution) => Unit): Option[Solution] = {
    log(vars)
    if (vars.size < csp.vars.size) {
      if (!csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
        val x = csp.domains(vars.size).head

        solve(vars :+ x, forwardCheck(vars.size, csp), log).orElse(
          solve(vars, forwardCheck(vars.size, csp.pruneDomain(vars.size, x)), log))
      } else {
        None
      }
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardChecking2waySolver extends ForwardChecking2waySolver
