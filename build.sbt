import AssemblyKeys._

assemblySettings

name := "SolventGreen"

version := "0.1"

scalaVersion := "2.10.0"

jarName in assembly := "solvent-green.jar"

mainClass in assembly := Some("solvent.green.benchmark.Benchmark")

scalacOptions := List("-feature")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
