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
import com.microsoft.z3.RatNum
import com.microsoft.z3.RealExpr
import com.microsoft.z3.Sort
import com.microsoft.z3.Status
import com.microsoft.z3.enumerations.Z3_lbool
import jurisk.process.Runner
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

// The https://github.com/tudo-aqua/z3-turnkey distribution did not work on task 2023-24 while the same SMT-LIB program
// worked from the command line. Thus, some methods have ended up being deprecated, and this class is mostly
// a way to generate SMT-LIB programs (see https://smtlib.cs.uiowa.edu/language.shtml).
//
// You could consider removing the Z3 dependency and just generating SMT-LIB format directly, but it's probably not
// worth it.
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

  @deprecated("Use `runExternal` instead", "2023-12-24")
  def checkAndGetModel(): Model

  // TODO: Make type-safe?
  def runExternal(evaluate: String*): List[String]
  def resultToInt(result: String): Int
  def resultToLong(result: String): Long

  def debugPrint(): Unit

  def maximize[R <: Sort](expr: Expr[R]): Optimize.Handle[R]
  def minimize[R <: Sort](expr: Expr[R]): Optimize.Handle[R]

  def realConstant(n: Int): RatNum
  def intConstant(n: Int): IntNum
  def longConstant(n: Long): IntNum

  def intToReal(n: Expr[IntSort]): RealExpr

  def labeledBool(label: String): BoolExpr
  def labeledReal(label: String): RealExpr
  def labeledInt(label: String): IntExpr

  def equal(a: Expr[_], b: Expr[_]): BoolExpr
  def greaterOrEqual[A <: ArithSort, B <: ArithSort](
    a: Expr[A],
    b: Expr[B],
  ): BoolExpr

  def greater[A <: ArithSort, B <: ArithSort](
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

  @deprecated("Use `runExternal` instead", "2023-12-24")
  def extractBoolean(b: BoolExpr): Option[Boolean]

  @deprecated("Use `runExternal` instead", "2023-12-24")
  def extractInt(n: IntExpr): Int

  @deprecated("Use `runExternal` instead", "2023-12-24")
  def extractLong(n: IntExpr): Long
}

private class Z3Optimizer(val context: Context, val optimize: Optimize)
    extends Optimizer {
  val Zero: IntNum     = intConstant(0)
  val One: IntNum      = intConstant(1)
  val MinusOne: IntNum = intConstant(-1)

  val False: BoolExpr = context.mkBool(false)
  val True: BoolExpr  = context.mkBool(true)

  def realConstant(n: Int): RatNum =
    context.mkReal(n)

  def intConstant(n: Int): IntNum =
    context.mkInt(n)

  def longConstant(n: Long): IntNum =
    context.mkInt(n)

  def intToReal(n: Expr[IntSort]): RealExpr =
    context.mkInt2Real(n)

  def labeledReal(label: String): RealExpr =
    context.mkRealConst(label)

  def labeledInt(label: String): IntExpr =
    context.mkIntConst(label)

  def labeledBool(label: String): BoolExpr =
    context.mkBoolConst(label)

  def equal(a: Expr[_], b: Expr[_]): BoolExpr =
    context.mkEq(a, b)

  def greater[A <: ArithSort, B <: ArithSort](
    a: Expr[A],
    b: Expr[B],
  ): BoolExpr =
    context.mkGt(a, b)

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
    assert(status == Status.SATISFIABLE, "Model is not satisfiable")
    optimize.getModel
  }

  def runExternal(evaluate: String*): List[String] = {
    val debug = false

    val programStart = optimize.toString

    // Note - `get-value` is SMT-LIB spec, but `eval` works with Z3
    val programEnd = evaluate
      .map { what =>
        s"(eval $what)"
      }
      .mkString("\n")

    val program = s"$programStart\n$programEnd\n(exit)"

    if (debug) println(program)

    val results = Runner.runSync("z3", "-in")(program)

    if (debug) println(results)

    val lines = results.splitLines
    lines.head.trim shouldEqual "sat"
    lines.tail.size shouldEqual evaluate.length
    lines.tail
  }

  def resultToInt(result: String): Int =
    result.trim match {
      case s"(- $n)" => -n.toInt
      case other     => other.toInt
    }

  def resultToLong(result: String): Long =
    result.trim match {
      case s"(- $n)" => -n.toLong
      case other     => other.toLong
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
      optimizer.intConstant(int)
  }

  implicit class RichLong(val long: Long) {
    def constant(implicit optimizer: Optimizer): IntExpr =
      optimizer.longConstant(long)
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

    def >(other: Expr[B])(implicit
      optimizer: Optimizer
    ): BoolExpr = optimizer.greater(expr, other)

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
