package me.katze.gui4s.layout
package linear

import scala.math.Fractional.Implicits.*
import cats.{Foldable, Traverse}
import cats.syntax.all.*
import me.katze.gui4s.geometry.*

def placeSpaceBetween[Container[_] : Traverse, MeasurementUnit: Fractional](sizes: Container[MeasurementUnit], space: MeasurementUnit): Container[Rect1dOnPoint1d[MeasurementUnit]] =
  val gap = spaceBetween(sizes, space)
  placeBeginMany(sizes.map(_ + gap)).map(sized => sized.copy(length = sized.length - gap))
end placeSpaceBetween

def spaceBetween[Container[_] : Traverse, MeasurementUnit: Fractional](sizes: Container[MeasurementUnit], space: MeasurementUnit): MeasurementUnit =
  val gapsCount = Fractional[MeasurementUnit].fromInt(sizes.size.toInt - 1)
  val allSize = sizes.sumAll
  val freeSpace = space - allSize
  freeSpace / gapsCount
end spaceBetween
