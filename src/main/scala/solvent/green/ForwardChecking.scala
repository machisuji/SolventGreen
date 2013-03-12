package solvent.green

trait ForwardChecking {
  /**
   * Prunes domains so that only values remain that satisfy a CSP's constraints
   * involving a given variable.
   *
   * @param varNum The index of the variable based on which the forward check is made.
   * @param csp The CSP to be pruned.
   * @return A new CSP with pruned domains.
   */
  def forwardCheck(varNum: Int, csp: CSP): CSP = {
    // allowed values derived from all constraints that go from some variable i to <varNum>
    val allAllowedBack = csp.constraints.filter(_.constrainedVars contains varNum).map(con =>
      con.varNum -> csp.domains(con.varNum).flatMap(v => con.allowedFor(v)))

    // varNums of variables that have constraints pointing to them from variable varNum
    val constrainedForth = csp.constraints.filter(_.varNum == varNum).flatMap(_.constrainedVars).toSet
    val allAllowedForth = csp.constraints.filter(_.varNum == varNum).map(con => // according allowed values
      csp.domains(varNum).flatMap(v => con.allowedFor(v)) // derive values for each possible value in domain
    ).flatten.groupBy(_._1).map(e => e._1 -> e._2.map(_._2).reduce(_ ++ _)) // consolidate

    val revisedDomains = csp.domains.zipWithIndex.map { case (domain, i) =>
      if (i == varNum) domain // forward checked var is not pruned
      else {
        val allowedBack = allAllowedBack.filter(_._1 == i) // allowed for currently checked variable i
        val allowedForth =
          if (allAllowedForth contains i) FineDomain(allAllowedForth(i)) // constrained, certain values allowed
          else if (constrainedForth contains i) FineDomain(Seq.empty) // constrained, but no values allowed
          else domain // not constrained, hence whole of domain allowed

        if (allowedBack.isEmpty && !constrainedForth.contains(i)) domain // no constraints defined for this domain
        else domain.pruneNot(v =>     // drop all values that cannot be derived from any of the
          allowedForth.contains(v) || // constraints pointing back/forth to/from variable <varNum>
            allowedBack.exists(_._2.exists(e => e._1 == varNum && e._2.contains(v))))
      }
    }

    csp.copy(domains = revisedDomains)
  }
}
