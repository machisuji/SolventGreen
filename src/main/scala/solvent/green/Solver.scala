package solvent.green

/**
 * CSP Solver
 */
trait Solver {

  type Solution = IndexedSeq[Int]

  def solve(csp: CSP): Option[Solution]

  def validate(solution: Solution, csp: CSP) =
    solution.size == csp.vars.size &&
    solution.zip(csp.domains).forall { case (value, domain) => domain contains value } &&
    checkConstraints(solution, csp)

  def checkConstraints(solution: Solution, csp: CSP) =
    csp.constraints.filter(con =>
      (con.varNum +: con.constrainedVars).forall(solution.size >)).forall(con =>
        con.allowedFor(solution(con.varNum)).forall{ case (col, values) =>
          values contains solution(col) })
}

case object BacktrackingSolver extends Solver {
  def solve(csp: CSP) = solve(IndexedSeq(), csp)

  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (vars.size < csp.vars.size && checkConstraints(vars, csp)) {
      csp.domains(vars.size).toIterator.map(x =>
        solve(vars :+ x, csp)).find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }
}
