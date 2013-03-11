package solvent.green

trait Backtracking2waySolver extends LoggingSolver {

  def solveAndLog(csp: CSP, log: (Solution, CSP) => Unit) = solve(IndexedSeq(), csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution, CSP) => Unit): Option[Solution] = {
    log(vars, csp)
    if (vars.size < csp.vars.size && !csp.domains.exists(_.isEmpty) && checkConstraints(vars, csp)) {
      val x = csp.domains(vars.size).head
      solve(vars :+ x, csp.assign(vars.size, x), log) orElse solve(vars, csp.prune(vars.size, x), log)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}

case object Backtracking2waySolver extends Backtracking2waySolver
