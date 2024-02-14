package reqt

import constraints.*

class TestConstr extends munit.FunSuite:

  test("IntVar relation constraints"):
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


  test("EnumVar String value relation constraints"):
    val x = EnumVar("Color", Seq("Red", "Black","White"))
    val cs = Seq(x === x.toInt("Black"))
    assert(cs == Seq(XeqC(x, x.toInt("Black"))))

