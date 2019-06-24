package spire.syntax.macros

import language.implicitConversions

import quoted._
import quoted.autolift._
import tasty.Reflection

def cforMacro[A](init: Expr[A], test: Expr[A => Boolean], next: Expr[A => A], body: Expr[A => Unit])
    given Type[A]: Expr[Unit] = '{
  var index = $init
  while (${ test('index) }) {
    ${ body('index) }
    index = ${ next('index) }
  }
}

def cforRangeMacro(r: Expr[Range], body: Expr[Int => Unit]) given Reflection : Expr[Unit] = {

  def isLiteral(t: Expr[Int]): Option[Int] = {
    val ref = the[Reflection]
    import ref._

    t.unseal match {
      case Literal(Constant.Int(n)) => Some(n)
      case _                        => None
    }
  }
  
  def strideUpUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index: Int = $fromExpr
    val limit: Int = $untilExpr
    while (index < limit) {
      ${ body('index) }
      index += $stride
    }
  }

  def strideUpTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index: Int = $fromExpr
    val end: Int = $untilExpr
    while (index <= end) {
      ${ body('index) }
      index += $stride
    }
  }

  def strideDownTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index: Int = $fromExpr
    val end: Int = $untilExpr
    while (index >= end) {
      ${ body('index) }
      index -= $stride
    }
  }

  def strideDownUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index: Int = $fromExpr
    val limit: Int = $untilExpr
    while (index > limit) {
      ${ body('index) }
      index -= $stride
    }
  }
  
  r match {
    case '{ ($i: Int) until $j } => strideUpUntil(i,j,1)
    case '{ ($i: Int) to $j }    => strideUpTo(i,j,1)

    case '{ ($i: Int) until $j by $step } =>
      isLiteral(step) match {
        case Some(k) if k > 0  => strideUpUntil(i,j,k)
        case Some(k) if k < 0  => strideDownUntil(i,j,-k)
        case Some(k) if k == 0 => QuoteError("zero stride", step)
        case _                 => '{ val b = $body; $r.foreach(b) }
      }

    case '{ ($i: Int) to $j by $step } =>
      isLiteral(step) match {
        case Some(k) if k > 0  => strideUpTo(i,j,k)
        case Some(k) if k < 0  => strideDownTo(i,j,-k)
        case Some(k) if k == 0 => QuoteError("zero stride", step)
        case _                 => '{ val b = $body; $r.foreach(b) }
      }

    case _ => '{ val b = $body; $r.foreach(b) }
  }
}
