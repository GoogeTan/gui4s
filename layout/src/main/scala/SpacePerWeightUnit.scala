package me.katze.gui4s.layout

import bound.{AxisDependentBounds, Bounds}

import cats.syntax.all.given

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

def spacePerWeightForContainerElements[MU : Fractional, T](
                                            elements : List[MaybeWeighted[Measurable[MU, T]]],
                                            constraints: AxisDependentBounds[MU]
                                          ) : SpacePerWeightUnit[MU] =
  val allTheWeight = elements.mapFilter(_.weight).sum
  val allTheSpace = constraints.mainAxis.maxValueUnsafe
  val nonWeightedElementsSpace = fixedSpace(elements, constraints.axis, constraints.bounds)
  if nonWeightedElementsSpace > allTheSpace then
    // Если фиксированные элементы заняли больше места, чем было свободного, то на взвешенные элементы места не остаётся.
    SpacePerWeightUnit(allTheWeight, Fractional[MU].zero)
  else
    val freeSpace = allTheSpace - nonWeightedElementsSpace
    SpacePerWeightUnit(allTheWeight, freeSpace)
  end if
end spacePerWeightForContainerElements

/**
 * Считает суммарный размер всех элементов без веса.
 */
def fixedSpace[MU : Numeric, T](children : List[MaybeWeighted[Measurable[MU,T]]], mainAxis : Axis, bounds : Bounds[MU]) : MU =
  children.map {
    case MaybeWeighted(None, value) =>
      value.placeInside(bounds).mainAxisValue(mainAxis)
    case _ => Numeric[MU].zero
  }.sum
end fixedSpace
