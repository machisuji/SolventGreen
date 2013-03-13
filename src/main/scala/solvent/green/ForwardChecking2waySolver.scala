package solvent.green

/**
 * Works like a Backtracking2waySolver only that forward checking is applied to
 * the CSP before branching.
 */
trait ForwardChecking2waySolver extends Backtracking2waySolver with ForwardChecking {
  override def left(i: Int, x: Int, csp: CSP) = forwardCheck(i, super.left(i, x, csp))
  override def right(i: Int, x: Int, csp: CSP) = forwardCheck(i, super.right(i, x, csp))
}

case object ForwardChecking2waySolver extends ForwardChecking2waySolver with Logging
