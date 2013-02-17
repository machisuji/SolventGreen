package solvent.green

trait ForwardCheckingSolver extends LoggingSolver {
  def solveAndLog(csp: CSP, log: (Solution) => Unit) = solve(IndexedSeq(), csp, log)

  def solve(vars: Solution, csp: CSP, log: (Solution) => Unit): Option[Solution] = {
    log(vars)
    if (vars.size < csp.vars.size && checkConstraints(vars, csp)) {
      csp.domains(vars.size).toIterator.map(x =>
        solve(vars :+ x, forwardCheck(vars.size, x, csp), log)).find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }

  def forwardCheck(varNum: Int, varValue: Int, csp: CSP): CSP = {
    val allowed = csp.constraints.filter(_.varNum == varNum).map(_.allowedFor(varValue)).flatten
    val constrainedDomains = allowed.groupBy(_._1).map(e =>
      e._1 -> Domain(csp.domains(e._1).toSeq intersect e._2.map(_._2).reduce(_ intersect _)))
    val revisedDomains = (0 until csp.domains.size).map(i =>
      constrainedDomains.getOrElse(i, csp.domains(i)))

    csp.copy(domains = revisedDomains)
  }
}

case object ForwardCheckingSolver extends ForwardCheckingSolver
