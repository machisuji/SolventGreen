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

  def time[R](action: => R): (R, Long) = {
    val ns = System.nanoTime
    val result = action

    (result, System.nanoTime - ns)
  }
}

case object BacktrackingSolver extends Solver {

  def solve(csp: CSP) = solve(IndexedSeq(), csp)

  def solve(vars: Solution, csp: CSP, log: (Solution) => Unit = _ => ()): Option[Solution] = {
    log(vars)
    if (vars.size < csp.vars.size && checkConstraints(vars, csp)) {
      csp.domains(vars.size).toIterator.map(x =>
        solve(vars :+ x, csp, log)).find(_.isDefined).getOrElse(None)
    }
    else if (validate(vars, csp)) Some(vars)
    else None
  }

  def solutionAndInfo(csp: CSP, show: Boolean = false) = {
    var calls: Int = 0
    def log(sol: Solution) {
      if (show) {
        println(sol)
      }
      calls += 1
    }
    val (result, ns) = time(solve(IndexedSeq(), csp, log))
    (result, calls, ns)
  }
}
