# SolventGreen

This is a collection of Constraint Satisfaction Problem (CSP) Solvers.
The library is absolutely randomly called "SolventGreen".
I haven't even seen Soylent Green, mind you. It's just the first thing that popped into my head.
Deal with it.

## Solving Strategies & Heuristics

Both *2-way branching* and *d-way branching* strategies are employed.
For both there is a *backtracking* solver and also a solver using *forward checking*.
Besides static *variable assignment order* there is also support for *smallest domain first* ordering.

## Building

Build with: sbt 0.11.3

## Packaging

> `sbt assembly`

## Running the Benchmark

> `java -jar target/solvent-green.jar solvent.green.benchmark.Benchmark`
