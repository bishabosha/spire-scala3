package spire.syntax.macros

import language.implicitConversions

import quoted._
import quoted.autolift._
import quoted.matching._
import tasty.Reflection

import collection.immutable.NumericRange

def cforMacro[A](init: Expr[A], test: Expr[A => Boolean], next: Expr[A => A], body: Expr[A => Unit])
    given Type[A]: Expr[Unit] = '{
  var index = $init
  while (${ test('index) }) {
    ${ body('index) }
    index = ${ next('index) }
  }
}

def cforRangeMacroLong(r: Expr[NumericRange[Long]], body: Expr[Long => Unit]) given Reflection : Expr[Unit] = {

  def strideUpUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = ($fromExpr)
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
        case Const(k) if k > 0  => strideUpUntil(i,j,k)
        case Const(k) if k < 0  => strideDownUntil(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)
        case _                  => '{ val b = $body; $r.foreach(b) }
      }

    case '{ ($i: Long) to $j by $step } =>
      step match {
        case Const(k) if k > 0  => strideUpTo(i,j,k)
        case Const(k) if k < 0  => strideDownTo(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)
        case _                  => '{ val b = $body; $r.foreach(b) }
      }

    case _ => '{ val b = $body; $r.foreach(b) }
  }
}

def cforRangeMacro(r: Expr[Range], body: Expr[Int => Unit]) given Reflection : Expr[Unit] = {
  
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
        case Const(k) if k > 0  => strideUpUntil(i,j,k)
        case Const(k) if k < 0  => strideDownUntil(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)
        case _                  => '{ val b = $body; $r.foreach(b) }
      }

    case '{ ($i: Int) to $j by $step } =>
      step match {
        case Const(k) if k > 0  => strideUpTo(i,j,k)
        case Const(k) if k < 0  => strideDownTo(i,j,-k)
        case Const(k) if k == 0 => QuoteError("zero stride", step)
        case _                  => '{ val b = $body; $r.foreach(b) }
      }

    case _ => '{ val b = $body; $r.foreach(b) }
  }
}
