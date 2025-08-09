package me.katze.gui4s.layout
package linear

def placeSpaceAround[T : Fractional](sizes : List[T], space : T) : List[Rect1dOnPoint1d[T]] =
  val zero = Fractional[T].zero
  val result = placeSpaceBetween((zero :: sizes) :+ zero, space)
  result.drop(1).dropRight(1)
end placeSpaceAround
