package jurisk

import com.microsoft.z3.{BoolSort, Context, Expr, IntNum, IntSort}

package object optimization {
  def Zero(implicit ctx: Context): IntNum     = ctx.mkInt(0)
  def One(implicit ctx: Context): IntNum      = ctx.mkInt(1)
  def MinusOne(implicit ctx: Context): IntNum = ctx.mkInt(-1)

  def abs(v: Expr[IntSort])(implicit ctx: Context): Expr[IntSort] =
    ctx.mkITE(ctx.mkGe(v, Zero), v, ctx.mkMul(v, MinusOne))

  def boolToInt(b: Expr[BoolSort])(implicit ctx: Context): Expr[IntSort] =
    ctx.mkITE(b, One, Zero)
}
