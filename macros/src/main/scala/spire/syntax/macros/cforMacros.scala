package spire.syntax.macros

import quoted._
import tasty.Reflection

def cforMacro[A](init: Expr[A], test: Expr[A => Boolean], next: Expr[A => A], body: Expr[A => Unit])
    given Type[A]: Expr[Unit] = '{
  var index = $init
  while (${ test('index) }) {
    ${ body('index) }
    index = ${ next('index) }
  }
}
