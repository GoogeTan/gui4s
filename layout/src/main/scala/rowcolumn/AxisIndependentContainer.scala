package me.katze.gui4s.layout
package rowcolumn

import bound.AxisDependentBounds

import scala.math.Fractional.*
import scala.math.Fractional.Implicits.*

def weightedRowColumnPlace[MU : Fractional, T](
                                                              mainAxis: Axis,
                                                              elements : List[MaybeWeighted[Measurable[MU, T]]],
                                                              rowColumnPlace : (List[Sized[MU, T]], AxisDependentBounds[MU]) => List[Placed[MU, T]]
                                                            ): Measurable[MU, List[Placed[MU, T]]] =
  constraints =>
    val dependentAxes = AxisDependentBounds.fromConstraints(constraints, mainAxis)
    val measured = measure(elements, dependentAxes)
    val placed = rowColumnPlace(measured, dependentAxes)
    val width = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MU].zero)
    val height = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MU].zero)
    Sized(placed, width, height)
end weightedRowColumnPlace
