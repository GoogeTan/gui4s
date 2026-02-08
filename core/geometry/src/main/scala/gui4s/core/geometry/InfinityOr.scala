package gui4s.core.geometry

final case class InfinityOr[+MeasurementUnit](value: Option[MeasurementUnit]):
  def this(value : MeasurementUnit) =
    this(Some(value))
  end this
  
  def isZero(using N : Numeric[? >: MeasurementUnit]) : Boolean = value.contains(N.zero)

  def minus[T >: MeasurementUnit](amount : T)(using N: Numeric[T]) : InfinityOr[T] =
    value match
      case Some(value) =>
        //TODO я не очень понимаю, почему тут ограничение на неотрицательность(да,
        // единственный юз это требует, но почему это ТУТ??!!!)
        if N.gteq(amount, value) then
          InfinityOr(Option(N.zero))
        else
          InfinityOr(Option(N.minus(value, amount)))
      case None => this
    end match
  end minus
  
  def *[T >: MeasurementUnit](another : InfinityOr[T])(using N : Numeric[T]) : InfinityOr[T] =
    InfinityOr(
      value.zip(another.value).map((a, b) => N.times(a, b))
    )
  end *
  
  def *[T >: MeasurementUnit](amount : T)(using N : Numeric[T]) : InfinityOr[T] =
    this * InfinityOr(Some(amount))
  end *
end InfinityOr