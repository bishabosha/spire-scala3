package spire.syntax.macros

import quoted._

import spire.algebra.{Field, CRing}

def fromRingImpl[A: Type](digits: Expr[String], radix: Expr[Int], A: Expr[CRing[A]])(using QuoteContext): Expr[A] =
  (digits -> radix): @unchecked match
  case Const(ds) -> Const(r) => fromBigIntImpl(BigInt(ds, r), A)
  case _                     => '{ $A.fromBigInt(BigInt($digits, $radix)) }

def fromFieldImpl[A: Type](digits: Expr[String], A: Expr[Field[A]])(using QuoteContext): Expr[A] =
  digits match
  case Const(ds) =>
    if floating.matches(ds) then
      val bigdec = BigDecimal(ds)
      if bigdec.isDecimalDouble || bigdec.isBinaryDouble || bigdec.isExactDouble then bigdec.toDouble match
        case 0.0 => '{ $A.zero                     }
        case 1.0 => '{ $A.one                      }
        case n   => '{ $A.fromDouble(${ Expr(n) }) }
      else
        '{ $A.fromBigDecimal(${ Expr(bigdec) }) }
    else
      fromBigIntImpl(BigInt(ds), A)

  case _ => '{ $A.fromBigDecimal(BigDecimal($digits)) }

private def fromBigIntImpl[A: Type](bigint: BigInt, A: Expr[CRing[A]])(using QuoteContext): Expr[A] =
  if bigint.isValidInt then bigint.toInt match
    case 0 => '{ $A.zero                  }
    case 1 => '{ $A.one                   }
    case n => '{ $A.fromInt(${ Expr(n) }) }
  else '{ $A.fromBigInt(${ Expr(bigint) }) }

private val floating = """.*[.eE].*""".r
