package solvent.green

import org.scalatest._
import matchers.ShouldMatchers
import OptionValues.convertOptionToValuable

class SolverTests extends FunSpec with ShouldMatchers {
  describe("BacktrackingSolver") {
    it("should solve the 4-Queens problem") {
      val csp = CSP fromFile "src/test/resources/csp/4Queens.csp"
      val sol = BacktrackingSolver solve csp

      sol.value.toList should equal (List(1, 3, 0, 2))
    }

    it("should solve the crystal maze (4) problem") {
      val csp = CSP fromFile "src/test/resources/csp/4crystalMaze.csp"
      val sol = BacktrackingSolver solve csp

      sol.value.toList should equal (List(3, 5, 7, 1, 8, 2, 4, 6))
    }

    it("should solve the crystal maze (6, for large domains) problem") {
      val csp = CSP fromFile "src/test/resources/csp/6crystalMaze.csp"
      val sol = BacktrackingSolver solve csp

      sol.value.toList should equal (List(1, 3, 5, 2, 4, 6, 8, 12, 7, 11, 13, 10, 14, 9))
    }

    it("should solve the langford (3) problem") {
      val csp = CSP fromFile "src/test/resources/csp/3langford.csp"
      val sol = BacktrackingSolver solve csp

      sol.value.toList should equal (List(2, 3, 1, 2, 1, 3))
    }
  }
}
