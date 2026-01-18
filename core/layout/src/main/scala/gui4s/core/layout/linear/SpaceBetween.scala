package gui4s.core.layout
package linear

import scala.math.Fractional.Implicits._

import cats.Traverse
import cats.syntax.all._

import gui4s.core.geometry._

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
