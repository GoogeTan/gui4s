package me.katze.gui4s.example
package api

// TODO Это по смыслу буквально то же самое, что и AxisBounds. Можно попытаться унифицировать.
enum Padding[+MeasurementUnit]:
  case Gap(gap : MeasurementUnit)
  case Fill extends Padding[Nothing]

  def gapOption : Option[MeasurementUnit] = this match
    case Padding.Fill => None
    case Padding.Gap(gap) => Some(gap)
  end gapOption

  def gapOrZero[AnotherMeasurementUnit >: MeasurementUnit](using N : Numeric[AnotherMeasurementUnit]) : AnotherMeasurementUnit =
    gapOption.getOrElse(N.zero)
  end gapOrZero
end Padding
