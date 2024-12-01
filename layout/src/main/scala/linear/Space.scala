package me.katze.gui4s.layout
package linear

import scala.math.Numeric.Implicits.given

def spaceBetweenElements[T: Numeric](elements : List[SizedElement[T]]): List[T] =
  def helper(previousEnd: T, starts : List[SizedElement[T]]) : List[T] =
    starts match
      case SizedElement(start, size) :: others =>
        (start - previousEnd) :: helper(start + size, others)
      case Nil => Nil
    end match
  end helper
  
  elements match
    case head :: tail =>
      helper(head.coordinateOfEnd, tail)
    case Nil => Nil
  end match
end spaceBetweenElements

def spaceAroundElements[T: Numeric](elements : List[SizedElement[T]], space : T): List[T] =
  elements match
    case firstElement :: (_ :+ lastElement) =>
      val between = spaceBetweenElements(elements)
      val endGap = space - lastElement.coordinateOfEnd
      (firstElement.coordinateOfStart :: between) :+ endGap
    case onlyOneElement :: Nil =>
      List(onlyOneElement.coordinateOfStart, space - onlyOneElement.coordinateOfEnd)
    case Nil => List(space)
end spaceAroundElements

def minimalRequiredSpace[T: Numeric](widgets: List[T]): T = widgets.sum

def spaceCovered[T : Numeric](elements : List[SizedElement[T]]) : CoveredSpace[T] =
  elements match
    case firstElement :: (_ :+ lastElement) =>
      CoveredSpace(firstElement.coordinateOfStart, lastElement.coordinateOfEnd)
    case onlyOneElement :: Nil =>
      CoveredSpace(onlyOneElement.coordinateOfStart, onlyOneElement.coordinateOfEnd)
    case Nil => CoveredSpace(Numeric[T].zero, Numeric[T].zero)
  end match
end spaceCovered

def beginEndGaps[T : Numeric](elements : List[SizedElement[T]], space : T) : (T, T) =
  val around = spaceAroundElements(elements, space)
  around match
    case firstElement :: (_ :+ lastElement) =>
      (firstElement, lastElement)
    case _ =>
      (Numeric[T].zero, space) // TODO может лучше вернуть option или кинуть исключение
  end match
end beginEndGaps
