package me.katze.gui4s.layout
package rowcolumn

import bound.AxisDependentBounds

import cats.Monad
import cats.syntax.all.*

import scala.math.Fractional.*
import scala.math.Fractional.Implicits.*

def weightedRowColumnPlace[F[+_] : Monad, MeasurementUnit : Fractional, T](
                                                                            mainAxis: Axis,
                                                                            elements : List[MaybeWeighted[Measurable[F, MeasurementUnit, T]]],
                                                                            rowColumnPlace : (List[Sized[MeasurementUnit, T]], AxisDependentBounds[MeasurementUnit]) => List[Placed[MeasurementUnit, T]]
                                                                          ): Measurable[F, MeasurementUnit, List[Placed[MeasurementUnit, T]]] =
  constraints =>
    val dependentAxes = AxisDependentBounds.fromConstraints(constraints, mainAxis)
    measure(elements, dependentAxes).map:
      measured =>
        val placed = rowColumnPlace(measured, dependentAxes)
        val width = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
        val height = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
        new Sized(placed, width, height)
end weightedRowColumnPlace
