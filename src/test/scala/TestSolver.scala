package reqt

import constr.*

class TestSolver extends munit.FunSuite:

  test("Simplest solve        "):
    assert(Var("x") == Var("x")) 

