package me.katze.gui4s.layout
package rowcolumn

import bound.AxisDependentBounds

import scala.math.Fractional.*
import scala.math.Fractional.Implicits.*

def weightedRowColumnPlace[MeasurementUnit : Fractional, T](
                                                              mainAxis: Axis,
                                                              elements : List[MaybeWeighted[Measurable[MeasurementUnit, T]]],
                                                              rowColumnPlace : (List[Sized[MeasurementUnit, T]], AxisDependentBounds[MeasurementUnit]) => List[Placed[MeasurementUnit, T]]
                                                            ): Measurable[MeasurementUnit, List[Placed[MeasurementUnit, T]]] =
  constraints =>
    val dependentAxes = AxisDependentBounds.fromConstraints(constraints, mainAxis)
    val measured = measure(elements, dependentAxes)
    val placed = rowColumnPlace(measured, dependentAxes)
    val width = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
    val height = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
    Sized(placed, width, height)
end weightedRowColumnPlace
