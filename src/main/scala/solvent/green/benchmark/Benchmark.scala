package solvent.green.benchmark

import solvent.green._

import language.implicitConversions

/**
 * Micro-Benchmark comparing the performance of Backtracking and ForwardChecking Solvers.
 */
object Benchmark extends App with Sugar {

  if (args.exists(arg => arg == "-h" || arg == "--help")) {
    println("Options:")
    println("  -q --quick | do not run expensive benchmarks")

    System.exit(0)
  }

  val problemNames: Seq[String] = Stream(
    "d-way_vs_2-way", "3Langford", "4crystalMaze", "6crystalMaze", "4Queens", "11Queens", "25Queens")

  val number = args.find(arg => arg == "-q" || arg == "--quick").map(_ =>
    problemNames.size - 1).getOrElse(problemNames.size)

  val problems: Seq[CSP] = problemNames.take(number).map(name =>
    CSP fromFile s"src/test/resources/csp/$name.csp")
  val solvers: Seq[LoggingSolver] = List(
    BacktrackingDwaySolver, Backtracking2waySolver, ForwardCheckingDwaySolver, ForwardChecking2waySolver)

  problems.zipWithIndex.foreach { case (problem, i) =>
    println("\n'" + problemNames(i) + "':")
    printf("%21s | %10s | %10s | %s\n", "Solver", "Nodes", "MS", "Result")
    solvers.foreach { solver =>
      val (result, nodes, ns) = 2.times.map(_ => solver solutionAndInfo problem).last
      def name = solver.getClass.getSimpleName.replace("Solver$", "")
      if (ns / 1000000 == 0) {
        printf("%21s | %10d | %10.2f | %s\n", name, nodes, ns / 1000000d, result)
      } else {
        printf("%21s | %10d | %10d | %s\n", name, nodes, ns / 1000000, result)
      }
    }
  }

  println()
}

trait Sugar {
  implicit class RicherInt(i: Int) {
    def times = 1 to i
  }
}
