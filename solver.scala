package reqt

import constraints.*  // depends on reqt.constr 
import org.jacop // depends on https://github.com/radsz/jacop/


/** An interface to the JaCoP solver http://www.jacop.eu  */
object solver:  
  import jacop.{constraints => jcon, core => jcore, search => jsearch}

  extension (cs: Seq[Constr]) 
    def solve(st: SearchType)(using cfg: SearchConfig): Result = 
      val result = new JacopSolver(cs, st, cfg).solve
      if result.conclusion != SolutionFound then cfg.warn(result.conclusion.toString)
      result
    
    def satisfy(using cfg: SearchConfig): Result             = solve(SolutionSearch.Satisfy)
    def countAll(using cfg: SearchConfig): Result            = solve(SolutionSearch.CountAll)
    def findAll(using cfg: SearchConfig): Result             = solve(SolutionSearch.FindAll)
    def maximize(cost: Var)(using cfg: SearchConfig): Result = solve(Maximize(cost))
    def minimize(cost: Var)(using cfg: SearchConfig): Result = solve(Minimize(cost))

  case class SearchConfig(
    valueSelection: ValueSelection = ValueSelection.IndomainRandom,
    variableSelection: VariableSelection = InputOrder,
    timeOutOption: Option[Long] = None,
    solutionLimitOption: Option[Int] = None,
    assignOption: Option[Seq[Var]] = None,
    defaultInterval: Range = -1000 to 1000,
    warn: String => Unit = (s: String) => println("WARNING: " + s),
    verbose: Boolean = false,
    debug: Boolean = false,
  )
  object SearchConfig:
    given defaultSearchConfig: SearchConfig = SearchConfig()

  case class Result(
    conclusion: Conclusion, 
    solutionCount: Int = 0, 
    lastSolution: Map[Var, Int] = Map[Var, Int](),
    interruptOption: Option[Interrupt] = None,
    solutionsOption: Option[Solutions] = None
  ):
    def get(v: Var): Option[Int] = lastSolution.get(v)


  enum Interrupt { case SearchTimeOut, SolutionLimitReached }
  export Interrupt.*

  enum Conclusion:
    case SolutionFound 
    case SolutionNotFound 
    case InconsistencyFound 
    case SearchFailed(msg: String) 
  export Conclusion.*

  sealed trait SearchType

  enum SolutionSearch extends SearchType: 
    case Satisfy
    case CountAll
    case FindAll 

  enum Optimize extends SearchType:
    def cost: Var
    case Minimize(cost: Var) 
    case Maximize(cost: Var) 
  export Optimize.*

  enum ValueSelection:
    case IndomainMax
    case IndomainMedian
    case IndomainMiddle
    case IndomainMin
    case IndomainRandom
    case IndomainSimpleRandom

  /* 
  value selection methods not yet implemented    
    IndomainSetMax  extends ValueSelection { def toJacop = jsearch.IndomainMax[JSetVar] }
    IndomainSetMin  extends ValueSelection { def toJacop = jsearch.IndomainMax[JSetVar] }
    IndomainSetRandom  extends ValueSelection { def toJacop = jsearch.IndomainMax[JSetVar] }
    IndomainHierarchical  extends ValueSelection 
    IndomainList  extends ValueSelection  
  */

  enum VariableSelection:
    case InputOrder
    case LargestDomain
    case SmallestDomain
    case LargestMax
    case LargestMin
    case SmallestMax
    case SmallestMin
    case MaxRegret
    case MostConstrainedDynamic
    case MostConstrainedStatic
  export VariableSelection.*

  /* variable selection methods not yet implemented:
    MaxCardDiff    
    MaxGlbCard    
    MaxLubCard    
    MinCardDiff    
    MinDomainOverDegree    
    MinGlbCard    
    MinLubCard    
    WeightedDegree    
  */  


  class Solutions( 
    val coreDomains: Array[Array[jcore.Domain]], 
    val coreVariables: Array[? <: jcore.Var],
    val nSolutions: Int,
    val lastSolution: Map[Var, Int]):

    private def toInt(d: jcore.Domain): Int = d.asInstanceOf[jcore.IntDomain].value

    val nVariables = coreVariables.length

    lazy val varMap: Map[String, Var] = lastSolution.keys.toSeq.map(v => (v.id.toString, v)).toMap

    lazy val variables: Seq[Var] = coreVariables.map(v => varMap(v.id)).toSeq

    lazy val indexOf: Map[Var, Int] = variables.zipWithIndex.toMap

    lazy val solutionMatrix: Seq[Seq[Int]] = coreDomains.slice(0,nSolutions-1).map(_.map(toInt).toSeq).toSeq

    def domain(solutionIndex: Int, variableIndex: Int): jcore.Domain = coreDomains(solutionIndex)(variableIndex)

    def value(solutionIndex: Int, variableIndex: Int): Int = toInt(domain(solutionIndex,variableIndex))

    def solution(solutionIndex: Int): Seq[Int] = coreDomains(solutionIndex).map(toInt).toSeq

    def solutionMap(solutionIndex: Int): Map[Var, Int] = 
      ( for i <- 0 until nVariables yield (variables(i), value(solutionIndex, i)) ) .toMap

    def allValuesOf(v: Var): Seq[Int] = ( for s <- 0 until nSolutions yield value(s, indexOf(v)) ) .toSeq   

    def forallSolutions(action: Map[Var, Int] => Unit): Unit = for i <- 0 until nSolutions do action(solutionMap(i))

    def mapSolutions[T](action: Map[Var, Int] => T): Seq[T] = for i <- 0 until nSolutions yield action(solutionMap(i))
      

    override def toString = s"Solutions([nSolutions=$nSolutions][nVariables=$nVariables])" 
  end Solutions

  object JacopSolver:
    type JIntVar = jcore.IntVar
    type JVar = jcore.Var
    type JBooleanVar = jcore.BooleanVar
    type Ivls = Map[Var, Seq[Range]]

    def distinctVars(cs: Seq[Constr]): Seq[Var] = cs.map { _.variables } .flatten.distinct

    def collectBounds(cs: Seq[Constr]): Seq[Bounds] = cs collect { case b: Bounds => b }
    
    def collectConstr(cs: Seq[Constr]): Seq[Constr] = cs filter { case b: Bounds => false ; case _ => true }
    
    def intervals(b: Bounds): Map[Var, Seq[Range]] = b.variables.map(v => (v, b.domain)).toMap
    
    def mergeIntervals(ivls1: Ivls, ivls2: Ivls): Ivls =
      var result = ivls1
      for (v, ivls) <- ivls2 do 
        if result.isDefinedAt(v) then result += v -> (result(v) ++ ivls) 
        else result += v -> ivls 
      result
    
    def nameToVarMap(vs: Seq[Var]): Map[String, Var] = vs.map(v => (v.id.toString, v)).toMap
    
    def checkUniqueToString(vs: Seq[Var]): Set[String] =
      val strings = vs.map(_.id.toString)
      strings.diff(strings.distinct).toSet
    
    def checkIfNameExists(name: String, vs: Seq[Var]): Boolean = 
      vs.exists { case Var(ref) => ref.toString == name }

    def flattenAllConstraints(cs: Seq[Constr]): Seq[Constr] =
      def flatten(xs: Seq[Constr]): Seq[Constr] = 
        if xs.isEmpty then xs 
        else (xs.head match {
          //case cs: Constraints => flatten(cs.value)  ???
          case c => Seq(c)
        } ) ++ flatten(xs.tail)
      flatten(cs)
    
    extension (vs: ValueSelection) def toJacop: jsearch.Indomain[JIntVar] = 
      import ValueSelection.*
      vs match
      case IndomainMax          => jsearch.IndomainMax[JIntVar]
      case IndomainMedian       => jsearch.IndomainMedian[JIntVar]
      case IndomainMiddle       => jsearch.IndomainMiddle[JIntVar]
      case IndomainMin          => jsearch.IndomainMin[JIntVar]
      case IndomainRandom       => jsearch.IndomainRandom[JIntVar]
      case IndomainSimpleRandom => jsearch.IndomainSimpleRandom[JIntVar]

    extension (vs: VariableSelection) 
      def toJacopVarSel(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection): jsearch.SelectChoicePoint[JIntVar] = 
        import VariableSelection.*
        vs match
        case InputOrder     => jsearch.InputOrderSelect[JIntVar](s, jvs, valSelect.toJacop) 
        case LargestDomain  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.LargestDomain(), valSelect.toJacop)
        case SmallestDomain => jsearch.SimpleSelect[JIntVar](jvs, jsearch.SmallestDomain(), valSelect.toJacop)
        case LargestMax  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.LargestMax(), valSelect.toJacop)
        case LargestMin  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.LargestMin(), valSelect.toJacop)
        case SmallestMax => jsearch.SimpleSelect[JIntVar](jvs, jsearch.SmallestMax(), valSelect.toJacop)
        case SmallestMin => jsearch.SimpleSelect[JIntVar](jvs, jsearch.SmallestMin(), valSelect.toJacop)
        case MaxRegret   => jsearch.SimpleSelect[JIntVar](jvs, jsearch.MaxRegret(), valSelect.toJacop)
        case MostConstrainedDynamic => jsearch.SimpleSelect[JIntVar](jvs, jsearch.MostConstrainedDynamic(), valSelect.toJacop)
        case MostConstrainedStatic  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.MostConstrainedStatic(), valSelect.toJacop)
    
    def toJCon(constr: Constr, store: jcore.Store, jIntVar: Map[Var, JIntVar]): jcon.Constraint =
      def jVarArray(vs: Seq[Var]) = vs.map(v => jIntVar(v)).toArray

      constr match
        case AbsXeqY(x, y) => jcon.AbsXeqY(jIntVar(x), jIntVar(y))
        case AllDifferent(vs) => jcon.Alldiff(jVarArray(vs))
        case And(cs) => 
          jcon.And(cs.map(c => 
              toJCon(c, store, jIntVar).asInstanceOf[jcon.PrimitiveConstraint]
            ).toArray
          )
        case Indexed(ix, vs, v)   => jcon.Element.choose(jIntVar(ix), jVarArray(vs), jIntVar(v))
        case SumEq(vs, x)         => jcon.SumInt(vs.map(v => jIntVar(v)).toArray, "==", jIntVar(x))
        case Count(vs, x, c)      => jcon.Count(vs.map(v => jIntVar(v)).toArray, jIntVar(x),  c)
        case XeqC(x, c)           => jcon.XeqC(jIntVar(x), c)
        case XeqY(x, y)           => jcon.XeqY(jIntVar(x), jIntVar(y))
        case XdivYeqZ(x, y, z)    => jcon.XdivYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XexpYeqZ(x, y, z)    => jcon.XexpYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XmulYeqZ(x, y, z)    => jcon.XmulYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XplusYeqZ(x, y, z)   => jcon.XplusYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XplusYlteqZ(x, y, z) => jcon.XplusYlteqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case Distance(x, y, z)    => jcon.Distance(jIntVar(x), jIntVar(y), jIntVar(z))
        case XgtC(x, c)           => jcon.XgtC(jIntVar(x), c)
        case XgteqC(x, c)         => jcon.XgteqC(jIntVar(x), c)
        case XgteqY(x, y)         => jcon.XgteqY(jIntVar(x), jIntVar(y))
        case XgtY(x, y)           => jcon.XgtY(jIntVar(x), jIntVar(y))
        case XltC(x, c)           => jcon.XltC(jIntVar(x), c)
        case XlteqC(x, c)         => jcon.XlteqC(jIntVar(x), c)
        case XlteqY(x, y)         => jcon.XlteqY(jIntVar(x), jIntVar(y))
        case XltY(x, y)           => jcon.XltY(jIntVar(x), jIntVar(y))
        case XneqC(x, c)          => jcon.XneqC(jIntVar(x), c)
        case XneqY(x, y)          => jcon.XneqY(jIntVar(x), jIntVar(y))
        case XeqBool(x, b)        => jcon.XeqC(jIntVar(x), if b then 1 else 0)
        case IfThen(c1, c2) =>
          val jc = (toJCon(c1, store, jIntVar), toJCon(c2, store, jIntVar)) 
          jcon.IfThen(jc._1.asInstanceOf[jcon.PrimitiveConstraint],   jc._2.asInstanceOf[jcon.PrimitiveConstraint])
        case IfThenElse(c1, c2, c3) =>
          val vs = Vector(c1, c2, c3)
          val jc = vs.map(toJCon(_, store, jIntVar).asInstanceOf[jcon.PrimitiveConstraint]) 
          jcon.IfThenElse(jc(0), jc(1), jc(2))
        case IfThenBool(x,y,z) => jcon.IfThenBool(jIntVar(x), jIntVar(y), jIntVar(z))
        case Reified(c1, x) =>
          val jc = toJCon(c1, store, jIntVar).asInstanceOf[jcon.PrimitiveConstraint]
          jcon.Reified(jc, jIntVar(x))
        case Diff2(rectangles) => 
          def matrix: Array[Array[JIntVar]] = rectangles.map(jVarArray(_)).toArray
          jcon.Diff2(matrix)
        case Binpacking(item, load, size) =>
          jcon.binpacking.Binpacking(jVarArray(item), jVarArray(load), size.toArray)
        case c => throw new NotImplementedError("unknown constraint: " + c)
    end toJCon

    
  end JacopSolver // companion object

  class JacopSolver(constraints: Seq[Constr], searchType: SearchType, cfg: SearchConfig):
    import JacopSolver.*
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
              case SolutionSearch.Satisfy => 
                setup(searchAll = false , recordSolutions = true )
                oneResult(label.labeling(store, selectChoicePoint)) 

              case SolutionSearch.CountAll => //count solutions but don't record any solution to save memory
                setup(searchAll = true , recordSolutions = false )
                countResult(label.labeling(store, selectChoicePoint), listener.solutionsNo) 

              case SolutionSearch.FindAll => 
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
    
  end JacopSolver // class
end solver

