package gui4s.core.widget.library.decorator

/**
 * @todo заменить на [[gui4s.core.geometry.InfinityOr]]
 */
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
