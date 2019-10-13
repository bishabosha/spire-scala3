# spire-scala3
Bringing Spire to Dotty/Scala 3, derived from `denisrosset/minispire`

13/10/2019
- Addition of generic number literals for types with `CRing` and `Field` given instances.
- default methods to make values with a `Field`: `fromDouble` and `fromBigDecimal`.

25/06/2019
- Reimplement all `cfor` macros, with added support for `NumericRange[Long]`.

11/06/2019
- Removal of macros and using inlined extension methods.
