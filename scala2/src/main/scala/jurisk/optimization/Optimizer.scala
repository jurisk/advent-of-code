package jurisk.optimization

import com.microsoft.z3.BoolSort
import com.microsoft.z3.Context
import com.microsoft.z3.Expr
import com.microsoft.z3.IntNum
import com.microsoft.z3.IntSort
import com.microsoft.z3.Optimize

trait Optimizer {
  val context: Context
  val optimize: Optimize

  val Zero: IntNum
  val One: IntNum
  val MinusOne: IntNum

  def abs(v: Expr[IntSort]): Expr[IntSort]
  def boolToInt(b: Expr[BoolSort]): Expr[IntSort]
}

private class Z3Optimizer(val context: Context, val optimize: Optimize) extends Optimizer {
  val Zero: IntNum     = context.mkInt(0)
  val One: IntNum      = context.mkInt(1)
  val MinusOne: IntNum = context.mkInt(-1)

  def abs(v: Expr[IntSort]): Expr[IntSort] =
    context.mkITE(context.mkGe(v, Zero), v, context.mkMul(v, MinusOne))

  def boolToInt(b: Expr[BoolSort]): Expr[IntSort] =
    context.mkITE(b, One, Zero)
}

object Optimizer {
  def z3(): Optimizer = {
    val context: Context = new Context
    val optimize = context.mkOptimize()

    new Z3Optimizer(context, optimize)
  }
}