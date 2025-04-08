package me.katze.gui4s.layout
package rowcolumn

import bound.AxisDependentBounds
import bound.withMaxValue

import cats.{Applicative, Monad}
import cats.syntax.all.*

def measure[F[+_] : Monad, MeasurementUnit : Fractional, T](
                                                                    children : List[MaybeWeighted[Measurable[F, MeasurementUnit, T]]],
                                                                    constraints: AxisDependentBounds[MeasurementUnit]
                                                                    ) : F[List[Sized[MeasurementUnit, T]]] =
  spacePerWeightForContainerElements(children, constraints)
    .flatMap(weightValue => measureWithWeight(children, weightValue, constraints))
end measure

def measureWithWeight[F[+_] : Applicative, MeasurementUnit: Fractional, T](
                                                                              children : List[MaybeWeighted[Measurable[F, MeasurementUnit, T]]],
                                                                              context: SpacePerWeightUnit[MeasurementUnit],
                                                                              constraints: AxisDependentBounds[MeasurementUnit]
                                                                              ) : F[List[Sized[MeasurementUnit, T]]] =
  children.traverse:
    case MaybeWeighted(None, value) =>
      value(constraints.bounds)
    case MaybeWeighted(Some(weight), value) =>
      value(constrainsWithWeight(weight, context, constraints).bounds)
end measureWithWeight

def constrainsWithWeight[MeasurementUnit : Fractional](
                                                        weight : Int,
                                                        context: SpacePerWeightUnit[MeasurementUnit],
                                                        constraints: AxisDependentBounds[MeasurementUnit]
                                                        ) : AxisDependentBounds[MeasurementUnit] =
  constraints.copy(mainAxis = constraints.mainAxis.withMaxValue(Some(context.spaceForWeight(weight))))
end constrainsWithWeight

