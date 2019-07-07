package spire.syntax.macros

import language.implicitConversions

import quoted._
import quoted.autolift._
import quoted.matching._
import tasty.Reflection
import collection.immutable.NumericRange

import spire.syntax.cfor.{RangeLike, RangeElem}

inline def cforInline[R](init: => R, test: => R => Boolean, next: => R => R, body: => R => Unit): Unit = {
  var index = init
  while (test(index)) {
    body(index)
    index = next(index)
  }
}

def cforRangeMacroGen[R <: RangeLike : Type](r: Expr[R], body: Expr[RangeElem[R] => Unit])
    given (ref: Reflection): Expr[Unit] = {
  import ref._

  type RangeL = NumericRange[Long]

  typeOf[R] match {
    case t if t <:< typeOf[Range]  => cforRangeMacro(r.cast[Range], body.cast[Int => Unit])
    case t if t <:< typeOf[RangeL] => cforRangeMacroLong(r.cast[RangeL], body.cast[Long => Unit])
    case t                         => QuoteError(s"Uneligable Range type ${t.show}", r)
  }
}

def cforRangeMacroLong(r: Expr[NumericRange[Long]], body: Expr[Long => Unit]) given (ref: Reflection) : Expr[Unit] = {
  import ref._

  def strideUpUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index < limit) {
      ${ body('index) }
      index += $stride
    }
  }

  def strideUpTo(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while (index <= end) {
      ${ body('index) }
      index += $stride
    }
  }

  def strideDownTo(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while (index >= end) {
      ${ body('index) }
      index -= $stride
    }
  }

  def strideDownUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index > limit) {
      ${ body('index) }
      index -= $stride
    }
  }

  r match {
    case '{ ($i: Long) until $j } => strideUpUntil(i,j,1)
    case '{ ($i: Long) to $j }    => strideUpTo(i,j,1)

    case '{ ($i: Long) until $j by $step } =>
      step match {
        case Const(k) if k  > 0 => strideUpUntil(i,j,k)
        case Const(k) if k  < 0 => strideDownUntil(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)

        case _ =>
          warning(s"defaulting to foreach, can not optimise non-constant step", step.unseal.pos)
          '{ val b = $body; $r.foreach(b) }
      }

    case '{ ($i: Long) to $j by $step } =>
      step match {
        case Const(k) if k  > 0 => strideUpTo(i,j,k)
        case Const(k) if k  < 0 => strideDownTo(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)

        case _ =>
          warning(s"defaulting to foreach, can not optimise non-constant step", step.unseal.pos)
          '{ val b = $body; $r.foreach(b) }
      }

    case _ =>
      warning(s"defaulting to foreach, can not optimise range expression", r.unseal.pos)
      '{ val b = $body; $r.foreach(b) }
  }
}

def cforRangeMacro(r: Expr[Range], body: Expr[Int => Unit]) given (ref: Reflection) : Expr[Unit] = {
  import ref._
  
  def strideUpUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index < limit) {
      ${ body('index) }
      index += $stride
    }
  }

  def strideUpTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while (index <= end) {
      ${ body('index) }
      index += $stride
    }
  }

  def strideDownTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val end   = $untilExpr
    while (index >= end) {
      ${ body('index) }
      index -= $stride
    }
  }

  def strideDownUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index > limit) {
      ${ body('index) }
      index -= $stride
    }
  }

  r match {
    case '{ ($i: Int) until $j } => strideUpUntil(i,j,1)
    case '{ ($i: Int) to $j }    => strideUpTo(i,j,1)

    case '{ ($i: Int) until $j by $step } =>
      step match {
        case Const(k) if k  > 0 => strideUpUntil(i,j,k)
        case Const(k) if k  < 0 => strideDownUntil(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)

        case _ =>
          warning(s"defaulting to foreach, can not optimise non-constant step", step.unseal.pos)
          '{ val b = $body; $r.foreach(b) }
      }

    case '{ ($i: Int) to $j by $step } =>
      step match {
        case Const(k) if k  > 0 => strideUpTo(i,j,k)
        case Const(k) if k  < 0 => strideDownTo(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)

        case _ =>
          warning(s"defaulting to foreach, can not optimise non-constant step", step.unseal.pos)
          '{ val b = $body; $r.foreach(b) }
      }

    case _ =>
      warning(s"defaulting to foreach, can not optimise range expression", r.unseal.pos)
      '{ val b = $body; $r.foreach(b) }
  }
}
