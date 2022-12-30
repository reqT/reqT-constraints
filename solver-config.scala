package reqt

case class SearchConfig(
  searchType: SearchType = Satisfy,
  valueSelection: ValueSelection = IndomainRandom,
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
  solutionsOption: Option[jacop.Solutions] = None
)  

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
export SolutionSearch.*

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
export ValueSelection.*
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
