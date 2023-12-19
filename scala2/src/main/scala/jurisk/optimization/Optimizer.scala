package jurisk.optimization

import cats.implicits.catsSyntaxOptionId
import com.microsoft.z3.ArithExpr
import com.microsoft.z3.ArithSort
import com.microsoft.z3.BoolExpr
import com.microsoft.z3.BoolSort
import com.microsoft.z3.Context
import com.microsoft.z3.Expr
import com.microsoft.z3.IntExpr
import com.microsoft.z3.IntNum
import com.microsoft.z3.IntSort
import com.microsoft.z3.Model
import com.microsoft.z3.Optimize
import com.microsoft.z3.Sort
import com.microsoft.z3.Status
import com.microsoft.z3.enumerations.Z3_lbool
import jurisk.utils.Parsing.StringOps

trait Optimizer {
  val context: Context
  val optimize: Optimize

  val Zero: IntNum
  val One: IntNum
  val MinusOne: IntNum

  val False: BoolExpr
  val True: BoolExpr

  def abs(v: Expr[IntSort]): Expr[IntSort]
  def boolToInt(b: Expr[BoolSort]): Expr[IntSort]

  def addConstraints(expressions: Expr[BoolSort]*): Unit

  def checkAndGetModel(): Model

  def debugPrint(): Unit

  def maximize[R <: Sort](expr: Expr[R]): Optimize.Handle[R]
  def minimize[R <: Sort](expr: Expr[R]): Optimize.Handle[R]

  def constant(n: Int): IntNum
  def constant(n: Long): IntNum

  def labeledBool(label: String): BoolExpr
  def labeledInt(label: String): IntExpr

  def equal(a: Expr[_], b: Expr[_]): BoolExpr
  def greaterOrEqual[A <: ArithSort, B <: ArithSort](
    a: Expr[A],
    b: Expr[B],
  ): BoolExpr
  def lessOrEqual[A <: ArithSort, B <: ArithSort](
    a: Expr[A],
    b: Expr[B],
  ): BoolExpr

  def add[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R]
  def sum[R <: ArithSort](expressions: Expr[R]*): ArithExpr[R]
  def sub[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R]

  def mul[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R]
  def div[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R]
  def rem(a: Expr[IntSort], b: Expr[IntSort]): IntExpr
  def implies(a: BoolExpr, b: BoolExpr): BoolExpr
  def and(expressions: Expr[BoolSort]*): BoolExpr
  def or(expressions: Expr[BoolSort]*): BoolExpr

  def extractBoolean(b: BoolExpr): Option[Boolean]
  def extractInt(n: IntExpr): Int
  def extractLong(n: IntExpr): Long
}

private class Z3Optimizer(val context: Context, val optimize: Optimize)
    extends Optimizer {
  val Zero: IntNum     = constant(0)
  val One: IntNum      = constant(1)
  val MinusOne: IntNum = constant(-1)

  val False: BoolExpr = context.mkBool(false)
  val True: BoolExpr  = context.mkBool(true)

  def constant(n: Int): IntNum =
    context.mkInt(n)

  def constant(n: Long): IntNum =
    context.mkInt(n)

  def labeledInt(label: String): IntExpr =
    context.mkIntConst(label)

  def labeledBool(label: String): BoolExpr =
    context.mkBoolConst(label)

  def equal(a: Expr[_], b: Expr[_]): BoolExpr =
    context.mkEq(a, b)

  def greaterOrEqual[A <: ArithSort, B <: ArithSort](
    a: Expr[A],
    b: Expr[B],
  ): BoolExpr =
    context.mkGe(a, b)

  def lessOrEqual[A <: ArithSort, B <: ArithSort](
    a: Expr[A],
    b: Expr[B],
  ): BoolExpr =
    context.mkLe(a, b)

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

  def add[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R] =
    sum(a, b)

  def sum[R <: ArithSort](expressions: Expr[R]*): ArithExpr[R] =
    context.mkAdd(expressions: _*)

  def sub[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R] =
    context.mkSub(a, b)

  def mul[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R] =
    context.mkMul(a, b)

  def div[R <: ArithSort](a: Expr[R], b: Expr[R]): ArithExpr[R] =
    context.mkDiv(a, b)

  def rem(a: Expr[IntSort], b: Expr[IntSort]): IntExpr =
    context.mkRem(a, b)

  def implies(a: BoolExpr, b: BoolExpr): BoolExpr =
    context.mkImplies(a, b)

  def and(expressions: Expr[BoolSort]*): BoolExpr =
    context.mkAnd(expressions: _*)

  def or(expressions: Expr[BoolSort]*): BoolExpr =
    context.mkOr(expressions: _*)

  def extractBoolean(b: BoolExpr): Option[Boolean] = {
    val bool = optimize.getModel.evaluate(b, true)
    bool.getBoolValue match {
      case Z3_lbool.Z3_L_FALSE => false.some
      case Z3_lbool.Z3_L_UNDEF => None
      case Z3_lbool.Z3_L_TRUE  => true.some
    }
  }

  def extractInt(n: IntExpr): Int = {
    val result = optimize.getModel.evaluate(n, true)
    result match {
      case intNum: IntNum => intNum.getInt
      case _              => s"Expected IntNum: $result".fail
    }
  }

  def extractLong(n: IntExpr): Long = {
    val result = optimize.getModel.evaluate(n, true)
    result match {
      case intNum: IntNum => intNum.getInt64
      case _              => s"Expected IntNum: $result".fail
    }
  }
}

object Optimizer {
  def z3(): Optimizer = {
    val context: Context = new Context
    val optimize         = context.mkOptimize()

    new Z3Optimizer(context, optimize)
  }
}

object ImplicitConversions {
  implicit class RichInt(val int: Int) {
    def constant(implicit optimizer: Optimizer): IntExpr =
      optimizer.constant(int)
  }

  implicit class RichLong(val long: Long) {
    def constant(implicit optimizer: Optimizer): IntExpr =
      optimizer.constant(long)
  }

  implicit class RichString(val string: String) {
    def labeledInt(implicit optimizer: Optimizer): IntExpr =
      optimizer.labeledInt(string)

    def labeledBool(implicit optimizer: Optimizer): BoolExpr =
      optimizer.labeledBool(string)
  }

  implicit class RichArithExprIntSort[B <: ArithSort](val expr: Expr[B]) {
    def +(other: Expr[B])(implicit optimizer: Optimizer): ArithExpr[B] =
      optimizer.add(expr, other)
    def -(other: Expr[B])(implicit optimizer: Optimizer): ArithExpr[B] =
      optimizer.sub(expr, other)
    def *(other: Expr[B])(implicit optimizer: Optimizer): ArithExpr[B] =
      optimizer.mul(expr, other)
    def /(other: Expr[B])(implicit optimizer: Optimizer): ArithExpr[B] =
      optimizer.div(expr, other)
    def >=(other: Expr[B])(implicit
      optimizer: Optimizer
    ): BoolExpr = optimizer.greaterOrEqual(expr, other)
    def <=(other: Expr[B])(implicit
      optimizer: Optimizer
    ): BoolExpr = optimizer.lessOrEqual(expr, other)
  }

  implicit class RichExprIntSort(val expr: Expr[IntSort]) {
    def %(other: Expr[IntSort])(implicit optimizer: Optimizer): IntExpr =
      optimizer.rem(expr, other)

    def abs(implicit optimizer: Optimizer): Expr[IntSort] = optimizer.abs(expr)
  }

  implicit class RichExprBoolSort(val expr: Expr[BoolSort]) {
    def toInt(implicit optimizer: Optimizer): Expr[IntSort] =
      optimizer.boolToInt(expr)
  }

  implicit class RichExpr(val expr: Expr[_]) {
    def ===(other: Expr[_])(implicit optimizer: Optimizer): BoolExpr =
      optimizer.equal(expr, other)
  }

  implicit class RichBoolExpr(val expr: BoolExpr) {
    def &&(other: BoolExpr)(implicit optimizer: Optimizer): BoolExpr  =
      optimizer.and(expr, other)
    def ||(other: BoolExpr)(implicit optimizer: Optimizer): BoolExpr  =
      optimizer.or(expr, other)
    def ==>(other: BoolExpr)(implicit optimizer: Optimizer): BoolExpr =
      optimizer.implies(expr, other)
  }
}