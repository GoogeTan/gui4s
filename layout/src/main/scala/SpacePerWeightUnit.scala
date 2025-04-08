package me.katze.gui4s.layout

import bound.{AxisDependentBounds, Bounds}

import cats.{Applicative, Monad, Monoid}
import cats.syntax.all.*

import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.Ordered.orderingToOrdered

/**
 * Обозначает количество места, которое занимает каждая единица веса.
 * @param allTheWeight Суммарный вес всех элементов в контейнере
 * @param freeSpace Остаток места после установки не взвешенных элементов.
 */
final case class SpacePerWeightUnit[T : Fractional](allTheWeight: Int, freeSpace: T):
  /**
   * Считает, сколько места занимает виджет с данным весом.
   * @param weight Вес элемента
   * @return Количество места, которое он занимает.
   */
  def spaceForWeight(weight: Int): T =
    Fractional[T].fromInt(weight) * (freeSpace / Fractional[T].fromInt(allTheWeight))
  end spaceForWeight
end SpacePerWeightUnit

def spacePerWeightForContainerElements[F[+_] : Monad, MeasurementUnit : Fractional, T](
                                                                                        elements : List[MaybeWeighted[Measurable[F, MeasurementUnit, T]]],
                                                                                        constraints: AxisDependentBounds[MeasurementUnit]
                                                                                        ) : F[SpacePerWeightUnit[MeasurementUnit]] =
  val allTheWeight = elements.mapFilter(_.weight).sum
  val allTheSpace = constraints.mainAxis.maxValueUnsafe
  fixedSpace(elements, constraints.axis, constraints.bounds).map:
    nonWeightedElementsSpace =>
      if nonWeightedElementsSpace > allTheSpace then
        // Если фиксированные элементы заняли больше места, чем было свободного, то на взвешенные элементы места не остаётся.
        SpacePerWeightUnit(allTheWeight, Fractional[MeasurementUnit].zero)
      else
        val freeSpace = allTheSpace - nonWeightedElementsSpace
        SpacePerWeightUnit(allTheWeight, freeSpace)
end spacePerWeightForContainerElements

/**
 * Считает суммарный размер всех элементов без веса.
 */
def fixedSpace[F[+_] : Monad, MeasurementUnit: Monoid as M, T](children : List[MaybeWeighted[Measurable[F, MeasurementUnit,T]]], mainAxis : Axis, bounds : Bounds[MeasurementUnit]) : F[MeasurementUnit] =
  children.foldMapM[F, MeasurementUnit]:
    case MaybeWeighted(None, value) =>
      value(bounds).map(_.mainAxisValue(mainAxis))
    case _ => M.empty.pure[F]
end fixedSpace
