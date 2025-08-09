package me.katze.gui4s.layout
package linear

import scala.math.Fractional.Implicits.*

def placeSpaceBetween[T : Fractional](sizes : List[T], space : T) : List[Rect1dOnPoint1d[T]] =
  val gap = spaceBetween(sizes, space)
  placeBeginMany(sizes.map(_ + gap)).map(sized => sized.copy(length = sized.length - gap))
end placeSpaceBetween

def spaceBetween[T : Fractional](sizes : List[T], space : T) : T =
  val gapsCount = Fractional[T].fromInt(sizes.size - 1)
  val allSize = sizes.sum
  val freeSpace = space - allSize
  freeSpace / gapsCount
end spaceBetween
