package reqt 

def vars(n: Int): Seq[Var] = for i <- 0 until n yield Var(i)
def vars(r: Range): Seq[Var] = for i <- r yield Var(i)
def varsOf[T](ids: T*): Seq[Var] = ids.map(Var.apply)

def varsBy[T](n: Int)(id: Int => T): Seq[Var] = for i <- 0 until n yield Var(id(i))
def varsBy[T](r: Range)(id: Int => T): Seq[Var] = for i <- r yield Var(id(i))

def forAll[T](xs: Seq[T])(f: T => Constr): Seq[Constr] = xs.map(f(_))

def forAll[T1, T2](x1s:Seq[T1], x2s: Seq[T2])(f: (T1, T2) => Constr): Seq[Constr] = 
  for (x1 <- x1s; x2 <- x2s) yield f(x1, x2)

def forAll[T1, T2, T3](x1s:Seq[T1], x2s: Seq[T2], x3s: Seq[T3])(f: (T1, T2, T3) => Constr): Seq[Constr] = 
  for (x1 <- x1s; x2 <- x2s; x3 <- x3s) yield f(x1, x2, x3)

def sumForAll[T](xs:Seq[T])(f: T => Var) = SumBuilder(xs.map(f(_)).toVector) 
def sumOf(xs: Var*) = SumBuilder(xs.toVector)

// case class Interval(min: Int, max: Int): 
//   if min > max then summon[SearchConfig].warn("Negative interval min > max: " + this )
//   def ::(v: Var): Bounds = Bounds(Seq(v), Seq(this))
//   def ::(vs: Seq[Var]): Bounds = Bounds(vs, Seq(this))
//   def ::(b: Bounds): Bounds = Bounds(b.seq1, b.domain ++ Seq(this))

extension (v: Var)
  infix def in(r: Range): Bounds = Bounds(Seq(v), Seq(r))
  infix def in(rs: Seq[Range]): Bounds = Bounds(Seq(v), rs)

extension (vs: Seq[Var])
  infix def in(r: Range): Bounds = Bounds(vs, Seq(r))
  infix def in(rs: Seq[Range]): Bounds = Bounds(vs, rs)

extension (b: Bounds)
  infix def in(r: Range): Bounds = b.copy(domain = b.domain :+ r)
  infix def in(rs: Seq[Range]): Bounds = b.copy(domain = b.domain ++ rs)

extension (cs: Seq[Constr])
  def vars: Seq[Var] = cs.flatMap(_.variables)

case class Var(id: Any):
  def ===(that: Var) = XeqY(this, that)
  def ===(const: Int) = XeqC(this, const)
  def ===(const: Boolean) = XeqBool(this, const)
  def ===(sumThat: SumBuilder) = SumEq(sumThat.vs, this) 
  def ===(mulThat: MulBuilder) = XmulYeqZ(mulThat.x, mulThat.y, this)
  def >(that: Var) = XgtY(this, that)  
  def >(const: Int) = XgtC(this, const)
  def >=(that: Var) = XgteqY(this, that)  
  def >=(const: Int) = XgteqC(this, const)
  def <(that: Var) = XltY(this, that)  
  def <(const: Int) = XltC(this, const)
  def <=(that: Var) = XlteqY(this, that)  
  def <=(const: Int) = XlteqC(this, const)
  def =/=(that: Var) = XneqY(this, that)
  def =/=(const: Int) = XneqC(this, const)
  def =/=(const: Boolean) = XeqBool(this, !const)
  
  def *(that: Var) = MulBuilder(this, that)  
  def +(that: Var) = PlusBuilder(this, that)  
  
case class SumBuilder(vs: Vector[Var]): 
  def ===(that: Var) = SumEq(vs, that) 

case class MulBuilder(x: Var, y: Var):
  def ===(z: Var) = XmulYeqZ(x, y, z)

case class PlusBuilder(x: Var, y: Var):
  def ===(z: Var) = XplusYeqZ(x, y, z)
  def <=(z: Var) = XplusYlteqZ(x, y, z)

object Sum:
  def apply(v: Var, vs: Var *) = SumBuilder(v +: vs.toVector)
  def apply(vs: Seq[Var]) = SumBuilder(vs.toVector)

trait Variables: 
  def variables: Seq[Var]  

trait Constr extends Variables

trait PrimitiveConstr extends Constr:  //marker trait to prevent wrong usage of jacob primitive constr
  def <=>(that: Var) = Reified(this, that)

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
  val constraints: Seq[Constr]
  lazy val variables: Seq[Var]  = constraints.flatMap(_.variables).distinct

trait CompoundConstr1 extends CompoundConstr:
  val c1: Constr
  val constraints = Seq(c1)

trait CompoundConstr2 extends CompoundConstr:
  val c1: Constr
  val c2: Constr
  val constraints = Seq(c1, c2)
trait CompoundConstr3 extends CompoundConstr:
  val c1: Constr
  val c2: Constr
  val c3: Constr
  val constraints = Seq(c1, c2, c3)

trait CompoundConstr1Var1 extends CompoundConstr1:
  val x: Var
  override lazy val variables: Seq[Var]  = (constraints.flatMap(_.variables) :+ x).distinct

case class Bounds(seq1: Seq[Var], domain: Seq[Range]) 
extends ConstrSeq1:
  def addDomainOf(that: Bounds): Bounds = Bounds(seq1, domain ++ that.domain)

object Bounds:
  def apply(v: Var, ivls: Range *) = new Bounds(Seq(v), ivls) 
  def apply(vs: Var *) = new Bounds(vs, Seq()) 

case class AbsXeqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr

case class AllDifferent(seq1: Seq[Var]) extends ConstrSeq1

case class And(constraints: Seq[Constr]) extends CompoundConstr
case object And: 
  def apply(c1: Constr, c2: Constr) = new And(Seq(c1, c2)) 

case class Indexed(index: Var, varSeq: Seq[Var], valueAtIndex: Var) extends Constr2Seq1:
  val x = index
  val y = valueAtIndex
  val seq1 = varSeq

case class SumEq(seq1: Seq[Var], x: Var) extends Constr1Seq1 
case class Count(seq1: Seq[Var], x: Var, c: Int) extends Constr1Seq1IntConst
case class XeqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr 
case class XeqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr 

case class XdivYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr
case class XexpYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr 
case class XmulYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr 
case class XplusYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr 
case class XplusYlteqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr
case class Distance(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr

case class XgtC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  //override def toScala = x.toScala + " > " + c
}
case class XgteqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr
case class XgteqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr 
case class XgtY(x: Var, y: Var) extends Constr2 with PrimitiveConstr 
case class XltC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr 
case class XlteqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr 
case class XlteqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr 
case class XltY(x: Var, y: Var) extends Constr2 with PrimitiveConstr 
case class XneqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr 
case class XneqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr 
case class XeqBool(x: Var, c: Boolean) extends Constr1BoolConst with PrimitiveConstr 

case class IfThen(c1: PrimitiveConstr, c2: PrimitiveConstr) extends CompoundConstr2 with PrimitiveConstr

case class IfThenElse(c1: PrimitiveConstr, c2: PrimitiveConstr, c3: PrimitiveConstr) extends CompoundConstr3 with PrimitiveConstr

case class IfThenBool(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr

case class Reified(c1: PrimitiveConstr, x: Var) extends  CompoundConstr1Var1 

case class Rectangle(x: Var, y: Var, dx: Var, dy: Var) extends Variables: // used in Diff2
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
