package solvent.green

trait ForwardCheckingDwaySolver extends LoggingSolver with ForwardChecking {
  def solveAndLog(csp: CSP, log: (Solution) => Unit) = solve(IndexedSeq(), csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution) => Unit): Option[Solution] = {
    log(vars)
    if (vars.size < csp.vars.size && !csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
      csp.domains(vars.size).toIterator.map(x =>
        solve(vars :+ x, forwardCheck(vars.size, csp.assign(vars.size, x)), log))
      .find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardCheckingDwaySolver extends ForwardCheckingDwaySolver
