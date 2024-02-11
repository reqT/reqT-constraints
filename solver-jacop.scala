package reqt

import org.jacop.{constraints => jcon, core => jcore, search => jsearch}
// http://www.jacop.eu/ 

import constr.*

object jacop:  
  extension (cs: Seq[Constr]) def solve(st: SearchType)(using cfg: SearchConfig): Result = 
    val result = JacopSolver(cs, st, cfg).solve
    if result.conclusion != SolutionFound then cfg.warn(result.conclusion.toString)
    result

  

  class Solutions( 
    val coreDomains: Array[Array[jcore.Domain]], 
    val coreVariables: Array[? <: jcore.Var],
    val nSolutions: Int,
    val lastSolution: Map[Var, Int]):

    lazy val nVariables = coreVariables.length

    lazy val varMap: Map[String, Var] = lastSolution.keys.toSeq.map(v => (v.id.toString, v)).toMap

    lazy val variables: Array[Var] = coreVariables.map(v => varMap(v.id))

    lazy val indexOf: Map[Var, Int] = variables.zipWithIndex.toMap

    private def toInt(d: jcore.Domain): Int = d.asInstanceOf[jcore.IntDomain].value

    def domain(solutionIndex: Int, variableIndex: Int): jcore.Domain = coreDomains(solutionIndex)(variableIndex)

    def value(s: Int, v: Int): Int = toInt(domain(s,v))

    def solution(s: Int): Array[Int] = coreDomains(s).map(toInt)

    def solutionMap(s: Int): Map[Var, Int] = 
      ( for i <- 0 until nVariables yield (variables(i), value(s, i)) ) .toMap

    def valueVector(v: Var): Vector[Int] = 
      ( for s <- 0 until nSolutions yield value(s, indexOf(v)) ) .toVector   

    def printSolutions: Unit = for i <- 0 until nSolutions do println(s"*** Solution $i:\n" + solutionMap(i))

    lazy val solutionMatrix: Array[Array[Int]] = coreDomains.slice(0,nSolutions-1).map(_.map(toInt))

    override def toString = s"Solutions([nSolutions=$nSolutions][nVariables=$nVariables])" 
  end Solutions

  type JIntVar = jcore.IntVar
  type JVar = jcore.Var
  type JBooleanVar = jcore.BooleanVar
  
  class JacopSolver(constraints: Seq[Constr], searchType: SearchType, cfg: SearchConfig):

    import SolverUtils.*

    import cfg.*
    
    lazy val flatConstr = flattenAllConstraints(constraints)
    lazy val domainOf: Map[Var, Seq[Range]] = buildDomainMap(flatConstr)

    def buildDomainMap(cs: Seq[Constr]): Ivls =
      var result = collectBounds(cs).map(intervals(_)).foldLeft(Map(): Ivls)(mergeIntervals(_,_))
      for v <- distinctVars(cs) do
        if !result.isDefinedAt(v) then result += v -> Seq(defaultInterval)
      result


    def varToJIntVar(v: Var, s: jcore.Store): JIntVar =
      val intDom = jcore.IntervalDomain()
      domainOf(v).foreach(ivl => intDom.addDom( new jcore.IntervalDomain(ivl.min, ivl.max)))
      new JIntVar(s, v.id.toString, intDom) 

    val minimizeHelpVarName = "_$minimize0"

    def collectIntVars(s: jcore.Store): Array[JIntVar] = 
      s.vars.collect { case x: JIntVar if (x.id != minimizeHelpVarName) => x } .toArray
            
    def solutionMap(s: jcore.Store, nameToVar: Map[String, Var] ): Map[Var, Int] = 
          collectIntVars(s).filter(_.singleton).map(iv => (nameToVar(iv.id), iv.value) ).toMap

    def solve: Result = if constraints.size > 0 then
      val store = new jcore.Store
      val vs = distinctVars(flatConstr)
      val cs = collectConstr(flatConstr)
      if verbose then println("*** VARIABLES:   " + vs.mkString(","))
      if verbose then println("*** CONSTRAINTS: " + cs.mkString(","))
      
      val duplicates = checkUniqueToString(vs)

      if !duplicates.isEmpty then 
        return Result(SearchFailed("Duplicate toString values of variables:" + 
          duplicates.mkString(", "))        )

      if checkIfNameExists(minimizeHelpVarName, vs) then 
        return Result(SearchFailed("Reserved variable name not allowed:" + minimizeHelpVarName))

      val intVarMap: Map[Var, JIntVar] = vs.map { v => (v, varToJIntVar(v, store)) } .toMap

      searchType match
        case opt: Optimize if (!intVarMap.isDefinedAt(opt.cost)) =>
          Result(SearchFailed("Cost variable not defined:" + opt.cost))
          
        case _ => 
          cs foreach { c => store.impose(toJCon(c, store, intVarMap)) }
          if debug then println(store)
          if !store.consistency then return Result(InconsistencyFound)
          val label = new jsearch.DepthFirstSearch[JIntVar]
          
          label.setPrintInfo(verbose) 
          
          def listener = label.getSolutionListener().asInstanceOf[jsearch.SimpleSolutionListener[JIntVar]]

          def setup(searchAll: Boolean, recordSolutions: Boolean): Unit = 
            //this must be done in the right order as JaCoP may overwrite solutionLimit if
            // searchAll is set after setting solutionLimit as explained by Kris
            listener.searchAll(searchAll)
            listener.recordSolutions(recordSolutions)
            timeOutOption.map { timeOut => label.setTimeOut(timeOut) } 
            solutionLimitOption.map { limit =>  listener.setSolutionLimit(limit) }
          
          val variablesToAssign: Array[JIntVar] = 
            if !assignOption.isDefined then collectIntVars(store) //assign all in store
            else assignOption.get.map(intVarMap(_)).toArray
          val selectChoicePoint = variableSelection.toJacopVarSel(store, variablesToAssign, valueSelection)
          def solutionNotFound = Result(SolutionNotFound)
          def solutionInStore = solutionMap(store, nameToVarMap(vs))
          def interruptOpt: Option[Interrupt] = 
            if label.timeOutOccured then Some(SearchTimeOut) 
            else if listener.solutionLimitReached && solutionLimitOption.isDefined then 
              Some(SolutionLimitReached)
            else None

          def mkSolutions(listener: jsearch.SimpleSolutionListener[JIntVar]): Solutions = 
            Solutions(
              listener.getSolutions(), 
              listener.getVariables(), 
              listener.solutionsNo(), 
              solutionInStore)

          def oneResult(ok: Boolean) = 
            if ok then 
              Result(SolutionFound, 1, solutionInStore, interruptOpt, Some(mkSolutions(listener)))  
            else solutionNotFound
          
          def countResult(ok: Boolean, i: Int) = 
            if ok then Result(SolutionFound, i, solutionInStore, interruptOpt, Some(mkSolutions(listener))) 
            else solutionNotFound
          
          val conclusion = 
            searchType match
              case Satisfy => 
                setup(searchAll = false , recordSolutions = true )
                oneResult(label.labeling(store, selectChoicePoint)) 

              case CountAll => //count solutions but don't record any solution to save memory
                setup(searchAll = true , recordSolutions = false )
                countResult(label.labeling(store, selectChoicePoint), listener.solutionsNo) 

              case FindAll => 
                setup(searchAll = true , recordSolutions = true )
                val found = label.labeling(store, selectChoicePoint)
                if !found then solutionNotFound else
                  val solutions = mkSolutions(listener)
                  Result(SolutionFound, solutions.nSolutions, solutionInStore, interruptOpt, Some(solutions))

              case Minimize(cost) => 
                setup(searchAll = false , recordSolutions = true )
                oneResult(label.labeling(store, selectChoicePoint, intVarMap(cost)))

              case Maximize(cost) =>  
                setup(searchAll = false , recordSolutions = true )
                val intDom = new jcore.IntervalDomain()
                domainOf(cost) foreach (ivl => intDom.addDom( new jcore.IntervalDomain(-ivl.max, -ivl.min)))
                val negCost = new JIntVar(store, minimizeHelpVarName, intDom)
                store.impose( new jcon.XmulCeqZ(intVarMap(cost), -1, negCost) )
                oneResult(label.labeling(store, selectChoicePoint, negCost))
            end match

          if verbose then println(store)
          conclusion
    else Result(SearchFailed("Empty constraints in argument to solve")) //end def solve
    
  end JacopSolver
end jacop

