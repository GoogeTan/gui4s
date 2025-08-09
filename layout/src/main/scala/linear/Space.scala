package me.katze.gui4s.layout
package linear

import cats.data.NonEmptyList

import scala.math.Numeric.Implicits.*

/**
 * Считает расстояние между элементами. Предполагается, что они расположены в порядке отдаления от начала координат в положительную строну.
 */
def spaceBetweenElements[MeasurementUnit: Numeric](items: NonEmptyList[Rect1dOnPoint1d[MeasurementUnit]]): List[MeasurementUnit] =
  def helper(previousItemsEnd: MeasurementUnit, remainingItems: List[Rect1dOnPoint1d[MeasurementUnit]]): List[MeasurementUnit] =
    remainingItems match
      case Rect1dOnPoint1d(size, start) :: others =>
        (start - previousItemsEnd) :: helper(start + size, others)
      case Nil => Nil
    end match
  end helper

  helper(items.head.coordinateOfTheEnd, items.tail)
end spaceBetweenElements

/**
 * Считает расстояние между элементами. Предполагается, что они расположены в порядке отдаления от начала координат в положительную строну.
 */
def spaceBetweenElements[MeasurementUnit: Numeric](items : List[Rect1dOnPoint1d[MeasurementUnit]]): List[MeasurementUnit] =
  items match
    case head :: tail => spaceBetweenElements(NonEmptyList(head, tail))
    case Nil => Nil
  end match
end spaceBetweenElements

/**
 * Считает расстояние от начала координат до первого элемента, между элементами и так же последнего элемента с концом доступного пространства.
 * Если элементов нет, то возвращает все доступное пространство.
 */
def spaceAroundElements[MeasurementUnit: Numeric as N](items: List[Rect1dOnPoint1d[MeasurementUnit]], availableSpace: MeasurementUnit): NonEmptyList[MeasurementUnit] =
  // Так как мы создали минимум 2 элемента в списке, то между ними точно есть расстояние, равное availableSpace. То есть, вернутся точно не пустой список.
  NonEmptyList.fromListUnsafe(
    spaceBetweenElements(
      (Rect1dOnPoint1d(N.zero, N.zero) :: items) :+ Rect1dOnPoint1d(N.zero, availableSpace)
    )
  )
end spaceAroundElements

def minimalRequiredSpace[MeasurementUnit: Numeric](lengths: List[MeasurementUnit]): MeasurementUnit =
  lengths.sum
end minimalRequiredSpace

def minimalRequiredSpace[MeasurementUnit: Numeric](lengths: NonEmptyList[MeasurementUnit]): MeasurementUnit =
  lengths.foldLeft(Numeric[MeasurementUnit].zero)(_ + _)
end minimalRequiredSpace

def spaceCovered[T : Numeric](items : List[Rect1dOnPoint1d[T]]) : Rect1dOnPoint1d[T] =
  items match
    case firstElement :: tail =>
      tail.lastOption.fold(
        Rect1dOnPoint1d.fromStartAndEnd(firstElement.coordinateOfTheBeginning, firstElement.coordinateOfTheEnd)
      )(lastElement =>
        Rect1dOnPoint1d.fromStartAndEnd(firstElement.coordinateOfTheBeginning, lastElement.coordinateOfTheEnd)
      )
    case Nil =>
      Rect1dOnPoint1d.empty
end spaceCovered

def beginEndGaps[T : Numeric](elements : NonEmptyList[Rect1dOnPoint1d[T]], space : T) : (T, T) =
  val around = spaceAroundElements(elements.toList, space)

  around.tail.lastOption match
    case Some(lastElement) =>
      (around.head, lastElement)
    case None =>
      (Numeric[T].zero, space) // TODO может лучше вернуть option или кинуть исключение
end beginEndGaps
