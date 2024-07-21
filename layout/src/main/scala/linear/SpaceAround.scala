package me.katze.gui4s.layout
package linear

def placeSpaceAround[T : Fractional](sizes : List[T], space : T) : List[T] =
  val zero = Fractional[T].zero
  val result = placeSpaceBetween(zero :: sizes.appended(zero), space)
  result.drop(1).dropRight(1)
end placeSpaceAround
