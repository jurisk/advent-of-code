package jurisk.optimization

import com.microsoft.z3.{
  BoolSort,
  Context,
  Expr,
  IntNum,
  IntSort,
  Model,
  Optimize,
  Sort,
  Status,
}

// TODO: Make the API more idiomatic
trait Optimizer {
  val context: Context
  val optimize: Optimize

  val Zero: IntNum
  val One: IntNum
  val MinusOne: IntNum

  def abs(v: Expr[IntSort]): Expr[IntSort]
  def boolToInt(b: Expr[BoolSort]): Expr[IntSort]

  def addConstraints(expressions: Expr[BoolSort]*): Unit

  def checkAndGetModel(): Model

  def debugPrint(): Unit

  def maximize[R <: Sort](expr: Expr[R]): Optimize.Handle[R]
  def minimize[R <: Sort](expr: Expr[R]): Optimize.Handle[R]
}

private class Z3Optimizer(val context: Context, val optimize: Optimize)
    extends Optimizer {
  val Zero: IntNum     = context.mkInt(0)
  val One: IntNum      = context.mkInt(1)
  val MinusOne: IntNum = context.mkInt(-1)

  def abs(v: Expr[IntSort]): Expr[IntSort] =
    context.mkITE(context.mkGe(v, Zero), v, context.mkMul(v, MinusOne))

  def boolToInt(b: Expr[BoolSort]): Expr[IntSort] =
    context.mkITE(b, One, Zero)

  def addConstraints(constraints: Expr[BoolSort]*): Unit =
    optimize.Add(constraints: _*)

  def checkAndGetModel(): Model = {
    val status = optimize.Check()
    assert(status == Status.SATISFIABLE)
    optimize.getModel
  }

  def debugPrint(): Unit =
    println(optimize)

  def maximize[R <: Sort](expr: Expr[R]): Optimize.Handle[R] =
    optimize.MkMaximize(expr)

  def minimize[R <: Sort](expr: Expr[R]): Optimize.Handle[R] =
    optimize.MkMinimize(expr)

}

object Optimizer {
  def z3(): Optimizer = {
    val context: Context = new Context
    val optimize         = context.mkOptimize()

    new Z3Optimizer(context, optimize)
  }
}
