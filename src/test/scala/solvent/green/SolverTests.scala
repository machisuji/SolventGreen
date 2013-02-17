package solvent.green

import org.scalatest._
import matchers.ShouldMatchers
import OptionValues.convertOptionToValuable

class SolverTests extends FunSpec with ShouldMatchers {

  val solvers = Seq(BacktrackingSolver, ForwardCheckingSolver)
  val problems = Map(
    "4Queens"       -> List(1, 3, 0, 2),
    "4crystalMaze"  -> List(3, 5, 7, 1, 8, 2, 4, 6),
    "6crystalMaze"  -> List(1, 3, 5, 2, 4, 6, 8, 12, 7, 11, 13, 10, 14, 9),
    "3langford"     -> List(2, 3, 1, 2, 1, 3))

  solvers.foreach { solver =>
    describe(solver.toString) {
      problems.foreach { case (problem, solution) =>
        it(s"should solve $problem") {
          val csp = CSP fromFile s"src/test/resources/csp/$problem.csp"
          val sol = solver solve csp

          sol.value.toList should equal (solution)
        }
      }
    }
  }
}
