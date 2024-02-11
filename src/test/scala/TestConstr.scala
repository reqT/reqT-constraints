package reqt

import constraints.*

class TestConstr extends munit.FunSuite:

  test("Single-variable relations, using constraints factory"):
    val cs = Seq(
      Var("x") === 42,
      Var("x") >= 42,
      Var("x") <= 42,
      Var("x") > 41,
      Var("x") < 43,
    )
    assert(cs ==  Seq(
      XeqC(Var("x"),42), 
      XgteqC(Var("x"),42), 
      XlteqC(Var("x"),42), 
      XgtC(Var("x"),41), 
      XltC(Var("x"),43),
    ))

  test("simple solve"):
    val cs = Seq(
      Var("x") === 42,
      Var("x") >= 42,
      Var("x") <= 42,
      Var("x") > 41,
      Var("x") < 43,
    )
    import solver.*
    val result = cs.satisfy
    assert(result.lastSolution(Var("x")) == 42) 
