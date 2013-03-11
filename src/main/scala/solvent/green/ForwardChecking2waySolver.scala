package solvent.green

trait ForwardChecking2waySolver extends LoggingSolver with ForwardChecking {
  def solveAndLog(csp: CSP, log: (Solution, CSP) => Unit) = solve(IndexedSeq(), csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution, CSP) => Unit): Option[Solution] = {
    log(vars, csp)
    if (vars.size < csp.vars.size && !csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
      val x = csp.domains(vars.size).head
      solve(vars :+ x, forwardCheck(vars.size, csp.assign(vars.size, x)), log).orElse( // <left | x == n>
        solve(vars, forwardCheck(vars.size, csp.prune(vars.size, x)), log)) // <right | x != n>
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object ForwardChecking2waySolver extends ForwardChecking2waySolver
