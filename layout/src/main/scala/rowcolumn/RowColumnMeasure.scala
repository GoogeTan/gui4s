package me.katze.gui4s.layout
package rowcolumn

import bound.AxisDependentBounds
import bound.withMaxValue

def measure[MU : Fractional, T](
                                  children : List[MaybeWeighted[Measurable[MU, T]]], 
                                  constraints: AxisDependentBounds[MU]
                                ) : List[Sized[MU, T]] =
  val weightValue = spacePerWeightForContainerElements(children, constraints)
  measureWithWeight(children, weightValue, constraints)
end measure

def measureWithWeight[MU: Fractional, T](
                                          children : List[MaybeWeighted[Measurable[MU, T]]], 
                                          context: SpacePerWeightUnit[MU], 
                                          constraints: AxisDependentBounds[MU]
                                        ) : List[Sized[MU, T]] =
  children.map:
    case MaybeWeighted(None, value) =>
      value.placeInside(constraints.bounds)
    case MaybeWeighted(Some(weight), value) =>
      value.placeInside(constrainsWithWeight(weight, context, constraints).bounds)
end measureWithWeight

def constrainsWithWeight[MU : Fractional](
                                            weight : Int,
                                            context: SpacePerWeightUnit[MU], 
                                            constraints: AxisDependentBounds[MU]
                                          ) : AxisDependentBounds[MU] =
  constraints.copy(mainAxis = constraints.mainAxis.withMaxValue(Some(context.spaceForWeight(weight))))
end constrainsWithWeight

