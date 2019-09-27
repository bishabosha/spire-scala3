package spire

import compiletime.erasedValue

erased def erasedTag[T]: Tag[T] = erasedValue[Tag[T]]

enum Tag[A] {

  case UnitTag     extends Tag[Unit]
  case BooleanTag  extends Tag[Boolean]
  case ByteTag     extends Tag[Byte]
  case ShortTag    extends Tag[Short]
  case IntTag      extends Tag[Int]
  case LongTag     extends Tag[Long]
  case FloatTag    extends Tag[Float]
  case DoubleTag   extends Tag[Double]
  case AnyTag[A]() extends Tag[A]

  def render[T]: String = this match {
    case UnitTag    => "Unit"
    case BooleanTag => "Boolean"
    case ByteTag    => "Byte"
    case ShortTag   => "Short"
    case IntTag     => "Int"
    case LongTag    => "Long"
    case FloatTag   => "Float"
    case DoubleTag  => "Double"
    case AnyTag()   => "Any"
  }
}

object Tag {

  given Tag[Unit]    = UnitTag
  given Tag[Boolean] = BooleanTag
  given Tag[Byte]    = ByteTag
  given Tag[Short]   = ShortTag
  given Tag[Int]     = IntTag
  given Tag[Long]    = LongTag
  given Tag[Float]   = FloatTag
  given Tag[Double]  = DoubleTag
  given [A]: Tag[A]  = AnyTag()

  inline def apply[T: Tag] = summon[Tag[T]]

}
