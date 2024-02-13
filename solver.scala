package reqt

import constraints.*

/** A generic interface to any constraint solver */
object solver:  
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
    def last(v: Var): Option[v.Value] = lastSolution.get(v).map(v.fromInt)

    def single(v: Var): Option[v.Value] = 
      if solutionCount == 1 && lastSolution.isDefinedAt(v) then Some(v.fromInt(lastSolution(v)))
      else None

    def all(v: Var): Seq[v.Value] = solutionsOption.map(ss => ss.allValuesOf(v).map(v.fromInt)).getOrElse(Seq())
  end Result


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

  trait Solutions: 
    def nSolutions: Int
    def lastSolution: Map[Var, Int]

    def nVariables: Int

    lazy val varMap: Map[String, Var] = lastSolution.keys.toSeq.map(v => (v.id.toString, v)).toMap

    def variables: Seq[Var]

    lazy val indexOf: Map[Var, Int] = variables.zipWithIndex.toMap

    def solutionMatrix: Seq[Seq[Int]]

    def value(solutionIndex: Int, variableIndex: Int): Int

    def solution(solutionIndex: Int): Seq[Int]

    def solutionMap(solutionIndex: Int): Map[Var, Int] = 
      ( for i <- 0 until nVariables yield (variables(i), value(solutionIndex, i)) ) .toMap

    def allValuesOf(v: Var): Seq[Int] = ( for s <- 0 until nSolutions yield value(s, indexOf(v)) ) .toSeq   

    def forallSolutions(action: Map[Var, Int] => Unit): Unit = for i <- 0 until nSolutions do action(solutionMap(i))

    def mapSolutions[T](action: Map[Var, Int] => T): Seq[T] = for i <- 0 until nSolutions yield action(solutionMap(i))

    override def toString = s"Solutions([nSolutions=$nSolutions][nVariables=$nVariables])" 
  end Solutions
end solver

