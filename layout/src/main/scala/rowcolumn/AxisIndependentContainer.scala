package me.katze.gui4s.layout
package rowcolumn

import bound.{AxisBounds, AxisDependentBounds}

import io.github.iltotore.iron.constraint.all.{*, given}
import io.github.iltotore.iron.constraint.collection.{*, given}
import io.github.iltotore.iron.{*, given}

def weightedColumn[MU : Fractional, T](
                          elements : List[MaybeWeighted[Measurable[MU, T]]],
                          rowColumnPlace : (List[Sized[MU, T]], AxisDependentBounds[MU]) => List[Placed[MU, T]],
                        )(using Layout[MU, T]) : Measurable[MU, T] =
  WeightedAxisBasedContainerMeasurable(Axis.Vertical, elements, rowColumnPlace)
end weightedColumn

def weightedRow[MU : Fractional, T](
  elements: List[MaybeWeighted[Measurable[MU, T]]],
  rowColumnPlace : (List[Sized[MU, T]], AxisDependentBounds[MU]) => List[Placed[MU, T]],
)(using Layout[MU, T]): Measurable[MU, T] =
  WeightedAxisBasedContainerMeasurable(Axis.Horizontal, elements, rowColumnPlace)
end weightedRow

def WeightedAxisBasedContainerMeasurable[MU : Fractional, T](
                                                              mainAxis: Axis,
                                                              elements : List[MaybeWeighted[Measurable[MU, T]]],
                                                              rowColumnPlace : (List[Sized[MU, T]], AxisDependentBounds[MU]) => List[Placed[MU, T]]
                                                            )(using Layout[MU, T]) : Measurable[MU, T] =
  constraints =>
    val dependentAxes = AxisDependentBounds.fromConstraints(constraints, mainAxis)
    val measured = measure(elements, dependentAxes)
    val placed = rowColumnPlace(measured, dependentAxes)
    summon[Layout[MU, T]](placed)
end WeightedAxisBasedContainerMeasurable
