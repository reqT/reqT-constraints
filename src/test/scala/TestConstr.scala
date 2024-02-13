package reqt

import constraints.*
import solver.*
import jacop.*

class TestConstr extends munit.FunSuite:

  test("Single-variable relations, using constraints factory"):
    val x = IntVar(id = "x")
    val cs = Seq(
      x === 42,
      x >= 42,
      x <= 42,
      x > 41,
      x < 43,
    )
    assert(cs ==  Seq(
      XeqC(x,42), 
      XgteqC(x,42), 
      XlteqC(x,42), 
      XgtC(x,41), 
      XltC(x,43),
    ))

  test("simple solve"):
    val x = IntVar(id = "x")
    val cs = Seq(
      x === 42,
      x >= 42,
      x <= 42,
      x > 41,
      x < 43,
    )
    val result = cs.satisfy
    assert(result.single(x) == Some(42)) 

  test("EnumVar using String"):
    val x = EnumVar("Color", Seq("Red", "Black","White"))
    val cs = Seq(x === x.toInt("Black"))
    val result: String = cs.satisfy.last(x).get
    assert(result ==  "Black")

  test("EnumVar using enum"):
    enum Color { case Red, Black, White}
    val x = EnumVar(Color, Color.values)
    val cs = Seq(x === x.toInt(Color.Black))
    val result: Color = cs.satisfy.last(x).get
    assert(result ==  Color.Black)

  test("EnumVar is bound by values"):
    val x = EnumVar("Col", Seq("B","C"))
    val result: Result = Seq(x === 42).satisfy
    assert(result.conclusion == Conclusion.InconsistencyFound)
