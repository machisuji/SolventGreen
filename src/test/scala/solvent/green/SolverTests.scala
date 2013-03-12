package solvent.green

import org.scalatest._
import matchers.ShouldMatchers
import OptionValues.convertOptionToValuable

class SolverTests extends FunSpec with ShouldMatchers {

  val solvers = Seq(
    BacktrackingDwaySolver, Backtracking2waySolver, ForwardCheckingDwaySolver, ForwardChecking2waySolver)
  val problems = Map(
    "4Queens"       -> Seq(1, 3, 0, 2),
    "4crystalMaze"  -> Seq(3, 5, 7, 1, 8, 2, 4, 6),
    "6crystalMaze"  -> Seq(1, 3, 5, 2, 4, 6, 8, 12, 7, 11, 13, 10, 14, 9),
    "3langford"     -> Seq(2, 3, 1, 2, 1, 3))

  solvers.foreach { solver =>
    describe(solver.toString) {
      problems.foreach { case (problem, solution) =>
        val csp = CSP fromFile s"src/test/resources/csp/$problem.csp"

        it(s"should solve $problem") {
          val sol = solver solve csp

          sol.value.toOrderedSeq should equal (solution)
        }

        it(s"should solve $problem with reversed variable ordering") {
          val sol = solver solve csp.reverseVarOrder

          sol.value.toOrderedSeq should equal (solution.reverse)
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

    it("should save work with certain variable orders") {
      // first variable assignment order example from slides (CS4402-1213-L8.pdf)
      val problem = CSP fromFile "src/test/resources/csp/var-order.csp"
      val solution = Seq(2, 9, 11, 10)
      val (sol1, nodes1, _) = Backtracking2waySolver solutionAndInfo problem.staticVarOrder(Seq(0, 1, 2, 3))
      val (sol2, nodes2, _) = Backtracking2waySolver solutionAndInfo problem.staticVarOrder(Seq(0, 1, 3, 2))

      sol1.value.toOrderedSeq should equal (solution)
      nodes1 should equal (21)

      sol2.value.toOrderedSeq should equal (solution)
      nodes2 should equal (13) // just as in the example in the slides
    }
  }
}
