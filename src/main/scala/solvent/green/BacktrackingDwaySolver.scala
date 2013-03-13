package solvent.green

trait BacktrackingDwaySolver extends Solver {
  def solve(vars: Solution, csp: CSP): Option[Solution] = {
    if (validIntermediate(vars, csp)) {
      csp.selectAllNext(vars).map(next => solve(vars + next, csp)).find(_.isDefined).getOrElse(None)
    }
    else if (valid(vars, csp)) Some(vars)
    else None
  }
}

case object BacktrackingDwaySolver extends BacktrackingDwaySolver with Logging
