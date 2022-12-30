package reqt

import org.jacop.{constraints => jcon, core => jcore, search => jsearch}
import jcore.{IntVar => JIntVar, Var => JVar, BooleanVar => JBooleanVar}

object SolverUtils:
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
      case c => println("Constr to JaCoP match error: " + c); ???

  extension (vs: ValueSelection) def toJacop: jsearch.Indomain[JIntVar] = vs match
    case IndomainMax          => jsearch.IndomainMax[JIntVar]
    case IndomainMedian       => jsearch.IndomainMedian[JIntVar]
    case IndomainMiddle       => jsearch.IndomainMiddle[JIntVar]
    case IndomainMin          => jsearch.IndomainMin[JIntVar]
    case IndomainRandom       => jsearch.IndomainRandom[JIntVar]
    case IndomainSimpleRandom => jsearch.IndomainSimpleRandom[JIntVar]

  extension (vs: VariableSelection) 
    def toJacopVarSel(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection): jsearch.SelectChoicePoint[JIntVar] = vs match
      case InputOrder     => jsearch.InputOrderSelect[JIntVar](s, jvs, valSelect.toJacop) 
      case LargestDomain  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.LargestDomain(), valSelect.toJacop)
      case SmallestDomain => jsearch.SimpleSelect[JIntVar](jvs, jsearch.SmallestDomain(), valSelect.toJacop)
      case LargestMax  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.LargestMax(), valSelect.toJacop)
      case LargestMin  => jsearch.SimpleSelect[JIntVar](jvs, jsearch.LargestMin(), valSelect.toJacop)
      case SmallestMax => jsearch.SimpleSelect[JIntVar](jvs, jsearch.SmallestMax(), valSelect.toJacop)
      case SmallestMin => jsearch.SimpleSelect[JIntVar](jvs, jsearch.SmallestMin(), valSelect.toJacop)
      case MaxRegret   => jsearch.SimpleSelect[JIntVar](jvs, jsearch.MaxRegret(), valSelect.toJacop)
      case MostConstrainedDynamic => 
        jsearch.SimpleSelect[JIntVar](jvs, jsearch.MostConstrainedDynamic(), valSelect.toJacop)
      case MostConstrainedStatic  =>
        jsearch.SimpleSelect[JIntVar](jvs, jsearch.MostConstrainedStatic(), valSelect.toJacop)

