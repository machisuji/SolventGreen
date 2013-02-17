package solvent.green

case object BacktrackingSolver extends LoggingSolver {

  def solveAndLog(csp: CSP, log: (Solution) => Unit) = solve(IndexedSeq(), csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution) => Unit): Option[Solution] = {
    log(vars)
    if (vars.size < csp.vars.size && checkConstraints(vars, csp)) {
      csp.domains(vars.size).toIterator.map(x =>
        solve(vars :+ x, csp, log)).find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}
