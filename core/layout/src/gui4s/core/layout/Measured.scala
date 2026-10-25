package gui4s.core.layout

final case class Measured[Size, Bounds, T](
  value : T,
  size : Size,
  bounds : Bounds
):
  def this(sized: Sized[Size, T], bounds : Bounds) =
    this(sized.value, sized.size, bounds)
  end this

  def asSized : Sized[Size, T] = Sized(value, size)

  def map[B](f : T => B) : Measured[Size, Bounds, B] =
    copy(value = f(value))
  end map
end Measured
