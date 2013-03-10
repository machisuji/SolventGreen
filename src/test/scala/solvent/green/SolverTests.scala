package solvent.green

import org.scalatest._
import matchers.ShouldMatchers
import OptionValues.convertOptionToValuable

class SolverTests extends FunSpec with ShouldMatchers {

  val solvers = Seq(BacktrackingDwaySolver, ForwardCheckingDwaySolver, ForwardChecking2waySolver)
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

  describe("2-way branching solver") {
    it("should save work when compared to d-way branching") {
      val problem = CSP fromFile "src/test/resources/csp/d-way_vs_2-way.csp"
      val (sol, nodes, _) = ForwardChecking2waySolver solutionAndInfo problem
      val (sol2, nodes2, _) = ForwardCheckingDwaySolver solutionAndInfo problem

      sol should not be ('defined)
      nodes should equal (5) // just as there are 5 nodes in the slides (CS4402_1213_L5.pdf - p. 49)

      sol2 should not be ('defined)
      nodes2 should equal (52) // as in p. 50
    }
  }
}
