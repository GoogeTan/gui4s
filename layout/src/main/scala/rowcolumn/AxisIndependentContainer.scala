package me.katze.gui4s.layout
package rowcolumn

import bound.{AxisBounds, AxisDependentBounds}

import io.github.iltotore.iron.constraint.all.{*, given}
import io.github.iltotore.iron.constraint.collection.{*, given}
import io.github.iltotore.iron.{*, given}
import scala.math.Fractional.{*, given}
import scala.math.Fractional.Implicits.{*, given}

def weightedRowColumnPlace[MU : Fractional, T](
                                                              mainAxis: Axis,
                                                              elements : List[MaybeWeighted[Measurable[MU, T]]],
                                                              rowColumnPlace : (List[Sized[MU, T]], AxisDependentBounds[MU]) => List[Placed[MU, T]]
                                                            ): Measurable[MU, List[Placed[MU, T]]] =
  constraints =>
    val dependentAxes = AxisDependentBounds.fromConstraints(constraints, mainAxis)
    val measured = measure(elements, dependentAxes)
    val placed = rowColumnPlace(measured, dependentAxes)
    val width = placed.map(a => a.x + a.width).max
    val height = placed.map(a => a.x + a.width).max
    Sized(placed, width, height)
end weightedRowColumnPlace
