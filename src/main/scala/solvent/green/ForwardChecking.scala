package solvent.green

trait ForwardChecking {
  def forwardCheck(varNum: Int, csp: CSP): CSP = {
    val allAllowedBack = csp.constraints.filter(_.constrainedVars contains varNum).map(con =>
      con.varNum -> csp.domains(con.varNum).flatMap(v => con.allowedFor(v)))

    val constrainedForth = csp.constraints.filter(_.varNum == varNum).flatMap(_.constrainedVars).toSet
    val allAllowedForth = csp.constraints.filter(_.varNum == varNum).map(con =>
      csp.domains(varNum).flatMap(v => con.allowedFor(v))
    ).flatten.toList.groupBy(_._1).map(e => e._1 -> e._2.map(_._2).reduce(_ ++ _))

    val revisedDomains = csp.domains.zipWithIndex.map { case (domain, i) =>
      if (i == varNum) domain // forward checked var is not pruned
      else {
        val allowedBack = allAllowedBack.filter(_._1 == i)
        val allowedForth =
          if (allAllowedForth contains i) FineDomain(allAllowedForth(i))
          else if (constrainedForth contains i) FineDomain(Seq.empty)
          else domain

        if (allowedBack.isEmpty && !constrainedForth.contains(i)) domain // no constraints defined for this domain
        else domain.pruneNot(v =>
          allowedForth.contains(v) ||
            allowedBack.exists(_._2.exists(e => e._1 == varNum && e._2.contains(v))))
      }
    }

    csp.copy(domains = revisedDomains)
  }
}
