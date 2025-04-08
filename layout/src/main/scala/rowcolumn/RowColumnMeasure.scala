package me.katze.gui4s.layout
package rowcolumn

import bound.AxisDependentBounds
import bound.withMaxValue

def measure[MeasurementUnit : Fractional, T](
                                              children : List[MaybeWeighted[Measurable[MeasurementUnit, T]]],
                                              constraints: AxisDependentBounds[MeasurementUnit]
                                              ) : List[Sized[MeasurementUnit, T]] =
  val weightValue = spacePerWeightForContainerElements(children, constraints)
  measureWithWeight(children, weightValue, constraints)
end measure

def measureWithWeight[MeasurementUnit: Fractional, T](
                                                        children : List[MaybeWeighted[Measurable[MeasurementUnit, T]]],
                                                        context: SpacePerWeightUnit[MeasurementUnit],
                                                        constraints: AxisDependentBounds[MeasurementUnit]
                                                      ) : List[Sized[MeasurementUnit, T]] =
  children.map:
    case MaybeWeighted(None, value) =>
      value.placeInside(constraints.bounds)
    case MaybeWeighted(Some(weight), value) =>
      value.placeInside(constrainsWithWeight(weight, context, constraints).bounds)
end measureWithWeight

def constrainsWithWeight[MeasurementUnit : Fractional](
                                                        weight : Int,
                                                        context: SpacePerWeightUnit[MeasurementUnit],
                                                        constraints: AxisDependentBounds[MeasurementUnit]
                                                        ) : AxisDependentBounds[MeasurementUnit] =
  constraints.copy(mainAxis = constraints.mainAxis.withMaxValue(Some(context.spaceForWeight(weight))))
end constrainsWithWeight

