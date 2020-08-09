package spire.syntax.macros

import quoted._
import collection.immutable.NumericRange

import spire.syntax.cfor.{RangeLike, RangeElem}

inline def cforInline[R](inline init: R, inline test: R => Boolean, inline next: R => R, inline body: R => Unit): Unit =
  var index = init
  while test(index) do
    body(index)
    index = next(index)

def cforRangeMacroGen[R <: RangeLike : Type](r: Expr[R], body: Expr[RangeElem[R] => Unit])(using qctx: QuoteContext): Expr[Unit] =
  import qctx._, tasty.{given _}

  type RangeL = NumericRange[Long]

  (r, body) match
  case '{$r: Range             } -> '{$body: (Int => Unit) } => cforRangeMacro(r, body)
  case '{$r: NumericRange[Long]} -> '{$body: (Long => Unit)} => cforRangeMacroLong(r, body)
  case '{$r: $t}                 -> _                        => report.error(s"Uneligable Range type ${t.show}", r); '{}

end cforRangeMacroGen

def cforRangeMacroLong(r: Expr[NumericRange[Long]], body: Expr[Long => Unit])(using qctx: QuoteContext): Expr[Unit] =
  import qctx._

  def strideUpUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while index < limit do
      ${ Expr.betaReduce(body)('index) }
      index += $stride
  }

  def strideUpTo(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while index <= end do
      ${ Expr.betaReduce(body)('index) }
      index += $stride
  }

  def strideDownTo(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while index >= end do
      ${ Expr.betaReduce(body)('index) }
      index -= $stride
  }

  def strideDownUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while index > limit do
      ${ Expr.betaReduce(body)('index) }
      index -= $stride
  }

  r match
  case '{ ($i: Long) until $j } => strideUpUntil(i,j,Expr(1L))
  case '{ ($i: Long) to $j }    => strideUpTo(i,j,Expr(1L))

  case '{ ($i: Long) until $j by $step } =>
    step match {
      case Const(k) if k  > 0 => strideUpUntil(i,j,Expr(k))
      case Const(k) if k  < 0 => strideDownUntil(i,j,Expr(-k))
      case Const(k) if k == 0 => report.error("zero stride", step); '{}

      case _ =>
        report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
        '{ val b = $body; $r.foreach(b) }
    }

  case '{ ($i: Long) to $j by $step } =>
    step match {
      case Const(k) if k  > 0 => strideUpTo(i,j,Expr(k))
      case Const(k) if k  < 0 => strideDownTo(i,j,Expr(-k))
      case Const(k) if k == 0 => report.error("zero stride", step); '{}

      case _ =>
        report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
        '{ val b = $body; $r.foreach(b) }
    }

  case _ =>
    report.warning(s"defaulting to foreach, can not optimise range expression", r)
    '{ val b = $body; $r.foreach(b) }

end cforRangeMacroLong

def cforRangeMacro(r: Expr[Range], body: Expr[Int => Unit])(using qctx: QuoteContext): Expr[Unit] =
  import qctx._

  def strideUpUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index < limit) {
      ${ Expr.betaReduce(body)('index) }
      index += $stride
    }
  }

  def strideUpTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while (index <= end) {
      ${ Expr.betaReduce(body)('index) }
      index += $stride
    }
  }

  def strideDownTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while (index >= end) {
      ${ Expr.betaReduce(body)('index) }
      index -= $stride
    }
  }

  def strideDownUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index > limit) {
      ${ Expr.betaReduce(body)('index) }
      index -= $stride
    }
  }

  r match
  case '{ ($i: Int) until $j } => strideUpUntil(i,j,Expr(1))
  case '{ ($i: Int) to $j }    => strideUpTo(i,j,Expr(1))

  case '{ ($i: Int) until $j by $step } =>
    step match {
      case Const(k) if k  > 0 => strideUpUntil(i,j,Expr(k))
      case Const(k) if k  < 0 => strideDownUntil(i,j,Expr(-k))
      case Const(k) if k == 0 => report.error("zero stride", step); '{}

      case _ =>
        report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
        '{ val b = $body; $r.foreach(b) }
    }

  case '{ ($i: Int) to $j by $step } =>
    step match {
      case Const(k) if k  > 0 => strideUpTo(i,j,Expr(k))
      case Const(k) if k  < 0 => strideDownTo(i,j,Expr(-k))
      case Const(k) if k == 0 => report.error("zero stride", step); '{}

      case _ =>
        report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
        '{ val b = $body; $r.foreach(b) }
    }

  case _ =>
    report.warning(s"defaulting to foreach, can not optimise range expression", r)
    '{ val b = $body; $r.foreach(b) }

end cforRangeMacro
