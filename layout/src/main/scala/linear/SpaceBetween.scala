package me.katze.gui4s.layout
package linear

import scala.math.Fractional.Implicits.*

def placeSpaceBetween[T : Fractional](sizes : List[T], space : T) : List[SizedElement[T]] =
  val gap = spaceBetween(sizes, space)
  val res = placeBeginMany(sizes.map(_ + gap)).map(sized => sized.copy(size = sized.size - gap))
  /*println(sizes)
  println(space)
  println(res)
  assert(res.lastOption.forall(_.coordinateOfEnd == space), "TODO FIX ME)*/
  res
end placeSpaceBetween

def spaceBetween[T : Fractional](sizes : List[T], space : T) : T =
  val gapsCount = Fractional[T].fromInt(sizes.size - 1)
  val allSize = sizes.sum
  val freeSpace = space - allSize
  freeSpace / gapsCount
end spaceBetween
