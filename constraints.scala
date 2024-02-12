package reqt 

/** A zero-dependency Scala-embedded DSL for expressing integer constraint problems. */
object constraints: 
  def constraints(cs: Constr*): Seq[Constr] = cs.toSeq 

  def intVars(n: Int): Seq[Var] = for i <- 0 until n yield IntVar(i)
  def intVars(r: Range): Seq[Var] = for i <- r yield IntVar(i)
  def intVarIds[T](ids: T*): Seq[Var] = ids.map(IntVar.apply)

  def varsBy[T](n: Int)(id: Int => T): Seq[Var] = for i <- 0 until n yield IntVar(id(i))
  def varsBy[T](r: Range)(id: Int => T): Seq[Var] = for i <- r yield IntVar(id(i))

  def forAll[T](xs: Seq[T])(f: T => Constr): Seq[Constr] = xs.map(f(_))

  def forAll[T1, T2](x1s:Seq[T1], x2s: Seq[T2])(f: (T1, T2) => Constr): Seq[Constr] = 
    for (x1 <- x1s; x2 <- x2s) yield f(x1, x2)

  def forAll[T1, T2, T3](x1s:Seq[T1], x2s: Seq[T2], x3s: Seq[T3])(f: (T1, T2, T3) => Constr): Seq[Constr] = 
    for (x1 <- x1s; x2 <- x2s; x3 <- x3s) yield f(x1, x2, x3)

  def sumForAll[T](xs:Seq[T])(f: T => Var) = SumBuilder(xs.map(f(_)).toVector) 

  extension (cs: Seq[Constr])
    def variables: Seq[Var] = cs.flatMap(_.variables)

  transparent trait HasVariables: 
    def variables: Seq[Var]  

  trait Constr extends HasVariables

  trait Var:
    type Id
    type Value
    def id: Id
    def fromInt(i: Int): Value
    def toInt(value: Value): Int
    def ===(y: Var): XeqY                  = XeqY(this, y)
    def ===(value: Int): XeqC              = XeqC(this, value)
    def ===(value: Boolean): XeqBool       = XeqBool(this, value)
    def ===(sumThat: SumBuilder): SumEq    = SumEq(sumThat.vs, this)
    def ===(mulThat: MulBuilder): XmulYeqZ = XmulYeqZ(mulThat.x, mulThat.y, this)

    def >(y: Var): XgtY                    = XgtY(this, y)
    def >(value: Int): XgtC                = XgtC(this, value)

    def >=(y: Var): XgteqY                 = XgteqY(this, y)
    def >=(value: Int): XgteqC             = XgteqC(this, value)

    def <(y: Var): XltY                    = XltY(this, y)  
    def <(value: Int): XltC                = XltC(this, value)

    def <=(y: Var): XlteqY                 = XlteqY(this, y)  
    def <=(value: Int): XlteqC             = XlteqC(this, value)

    def =/=(y: Var): XneqY                 = XneqY(this, y)
    def =/=(value: Int): XneqC             = XneqC(this, value)
    def =/=(value: Boolean): XeqBool       = XeqBool(this, !value)
    
    def *(y: Var) = MulBuilder(this, y)  
    def +(y: Var) = PlusBuilder(this, y)  

  extension (i: Int) def toValueOf(v: Var): v.Value = v.fromInt(i)
  case class IntVar[T](id: T) extends Var:
    type Id = T
    type Value = Int
    override def fromInt(i: Int): Int = i
    override def toInt(value: Int): Int = value
  end IntVar

  case class EnumVar[T, U](id: T, values: Seq[U]) extends Var:
    type Id = T
    type Value = U
    override def fromInt(i: Int): U = values(i)
    lazy val valueToIndex: Map[U, Int] = values.zipWithIndex.toMap
    override def toInt(value: U): Int = valueToIndex(value)
  object EnumVar:
    def apply[T, U](id: T, values: Array[U]): EnumVar[T, U] = new EnumVar(id, values.toSeq)

  case class SumBuilder(vs: Vector[Var]): 
    def ===(y: Var): SumEq = SumEq(vs, y)
 
  case class MulBuilder(x: Var, y: Var):
    def ===(z: Var): XmulYeqZ = XmulYeqZ(x, y, z)

  case class PlusBuilder(x: Var, y: Var):
    def ===(z: Var): XplusYeqZ = XplusYeqZ(x, y, z)
    def <=(z: Var): XplusYlteqZ = XplusYlteqZ(x, y, z)

  def sum(v: Var, vs: Var *): SumBuilder = SumBuilder(v +: vs.toVector)
  def sum(vs: Seq[Var]): SumBuilder      = SumBuilder(vs.toVector)

  trait PrimitiveConstr extends Constr:  // constraints that can be used as arguments in logical and conditional constraints
    def <=>(y: Var): Reified = Reified(this, y)

  trait Constr1IntConst extends Constr: 
    val x: Var
    val c: Int
    val variables: Seq[Var] = Seq(x) 

  trait Constr1BoolConst extends Constr: 
    val x: Var
    val c: Boolean
    val variables: Seq[Var] = Seq(x) 

  trait Constr2 extends Constr: 
    val x: Var; val y: Var
    val variables: Seq[Var] = Seq(x, y) 

  trait Constr3 extends Constr: 
    val x: Var; val y: Var; val z: Var
    val variables: Seq[Var] = Seq(x, y, z) 
  trait ConstrSeq1 extends Constr: 
    val seq1: Seq[Var]
    val variables: Seq[Var] = seq1

  trait Constr1Seq1 extends Constr: 
    val x: Var
    val seq1: Seq[Var]
    val variables: Seq[Var] = Seq(x) ++ seq1 

  trait Constr1Seq1IntConst extends Constr: 
    val x: Var
    val seq1: Seq[Var]
    val c: Int
    val variables: Seq[Var] = Seq(x) ++ seq1 

  trait Constr2Seq1 extends Constr: 
    val x: Var; val y: Var
    val seq1: Seq[Var]
    val variables: Seq[Var] = Seq(x, y) ++ seq1

  trait ConstrSeq2ConstSeq1 extends Constr: 
    def seq1: Seq[Var]
    def seq2: Seq[Var]
    def constSeq1: Seq[Int]
    lazy val variables: Seq[Var] = seq1 ++ seq2

  trait ConstrMatrix extends Constr: 
    def matrix: Vector[Vector[Var]]
    val variables: Seq[Var] = matrix.flatten

  trait CompoundConstr extends Constr:
    val constrSeq: Seq[Constr]
    lazy val variables: Seq[Var]  = constrSeq.flatMap(_.variables).distinct

  trait CompoundConstr1 extends CompoundConstr:
    val c1: Constr
    val constrSeq = Seq(c1)

  trait CompoundConstr2 extends CompoundConstr:
    val c1: Constr
    val c2: Constr
    val constrSeq = Seq(c1, c2)
  trait CompoundConstr3 extends CompoundConstr:
    val c1: Constr
    val c2: Constr
    val c3: Constr
    val constrSeq = Seq(c1, c2, c3)

  trait CompoundConstr1Var1 extends CompoundConstr1:
    val x: Var
    override lazy val variables: Seq[Var]  = (constrSeq.flatMap(_.variables) :+ x).distinct

  case class Bounds(seq1: Seq[Var], domain: Seq[Range]) extends ConstrSeq1

  extension (v: Var)
    infix def in(r: Range): Constr = Bounds(Seq(v), Seq(r))
    infix def in(rs: Seq[Range]): Constr = Bounds(Seq(v), rs)

  extension (vs: Seq[Var])
    infix def in(r: Range): Constr = Bounds(vs, Seq(r))
    infix def in(rs: Seq[Range]): Constr = Bounds(vs, rs)

  extension (b: Bounds)
    infix def in(r: Range): Constr = b.copy(domain = b.domain :+ r)
    infix def in(rs: Seq[Range]): Constr = b.copy(domain = b.domain ++ rs)

  case class AbsXeqY(x: Var, y: Var) extends Constr2, PrimitiveConstr

  case class AllDifferent(seq1: Seq[Var]) extends ConstrSeq1

  case class And(constrSeq: Seq[Constr]) extends CompoundConstr
  case object And: 
    def apply(c1: Constr, c2: Constr) = new And(Seq(c1, c2))

  case class Indexed(index: Var, varSeq: Seq[Var], valueAtIndex: Var) extends Constr2Seq1:
    val x = index
    val y = valueAtIndex
    val seq1 = varSeq

  case class SumEq(seq1: Seq[Var], x: Var) extends Constr1Seq1 
  case class Count(seq1: Seq[Var], x: Var, c: Int) extends Constr1Seq1IntConst
  case class XeqC(x: Var, c: Int) extends Constr1IntConst, PrimitiveConstr 

  case class XeqY(x: Var, y: Var) extends Constr2, PrimitiveConstr

  case class XdivYeqZ(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr
  case class XexpYeqZ(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr 
  case class XmulYeqZ(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr 
  case class XplusYeqZ(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr 
  case class XplusYlteqZ(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr
  case class Distance(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr

  case class XgtC(x: Var, c: Int) extends Constr1IntConst, PrimitiveConstr 
  
  case class XgteqC(x: Var, c: Int) extends Constr1IntConst, PrimitiveConstr
  case class XgteqY(x: Var, y: Var) extends Constr2, PrimitiveConstr
  case class XgtY(x: Var, y: Var) extends Constr2, PrimitiveConstr
  case class XltC(x: Var, c: Int) extends Constr1IntConst, PrimitiveConstr
  case class XlteqC(x: Var, c: Int) extends Constr1IntConst, PrimitiveConstr 
  case class XlteqY(x: Var, y: Var) extends Constr2, PrimitiveConstr
  case class XltY(x: Var, y: Var) extends Constr2, PrimitiveConstr
  case class XneqC(x: Var, c: Int) extends Constr1IntConst, PrimitiveConstr
  case class XneqY(x: Var, y: Var) extends Constr2, PrimitiveConstr
  case class XeqBool(x: Var, c: Boolean) extends Constr1BoolConst, PrimitiveConstr 

  case class IfThen(c1: PrimitiveConstr, c2: PrimitiveConstr) extends CompoundConstr2, PrimitiveConstr

  case class IfThenElse(c1: PrimitiveConstr, c2: PrimitiveConstr, c3: PrimitiveConstr) extends CompoundConstr3, PrimitiveConstr

  case class IfThenBool(x: Var, y: Var, z: Var) extends Constr3, PrimitiveConstr

  case class Reified(c1: PrimitiveConstr, x: Var) extends CompoundConstr1Var1

  case class Rectangle(x: Var, y: Var, dx: Var, dy: Var) extends HasVariables: // used in Diff2
    lazy val toVector: Vector[Var] = Vector(x, y, dx, dy)
    lazy val variables: Seq[Var] = toVector

  case class Diff2(rectangles: Vector[Vector[Var]]) extends ConstrMatrix:
    lazy val matrix = rectangles     
    assert(rectangles.map(_.size).distinct == Vector(4), "size of all rectangle vectors must be 4")
  object Diff2:
    def apply(rectangles: Rectangle *) = new Diff2(rectangles.toVector.map(_.toVector))

  case class Binpacking(item: Vector[Var], load: Vector[Var], size: Vector[Int]) 
      extends ConstrSeq2ConstSeq1:
    lazy val seq1 = item
    lazy val seq2 = load
    lazy val constSeq1 = size    

end constraints
