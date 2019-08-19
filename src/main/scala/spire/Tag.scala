package spire

import compiletime.erasedValue

erased def erasedTag[T]: Tag[T] = erasedValue[Tag[T]]

enum Tag[A] {

  case UnitTag    extends Tag[Unit]
  case BooleanTag extends Tag[Boolean]
  case ByteTag    extends Tag[Byte]
  case ShortTag   extends Tag[Short]
  case IntTag     extends Tag[Int]
  case LongTag    extends Tag[Long]
  case FloatTag   extends Tag[Float]
  case DoubleTag  extends Tag[Double]
  case AnyTag[A]  extends Tag[A]

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

  given as Tag[Unit]    = UnitTag
  given as Tag[Boolean] = BooleanTag
  given as Tag[Byte]    = ByteTag
  given as Tag[Short]   = ShortTag
  given as Tag[Int]     = IntTag
  given as Tag[Long]    = LongTag
  given as Tag[Float]   = FloatTag
  given as Tag[Double]  = DoubleTag
  given [A] as Tag[A]   = AnyTag()

  inline def apply[T: Tag] = the[Tag[T]]

}