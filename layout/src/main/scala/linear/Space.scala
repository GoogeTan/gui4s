package me.katze.gui4s.layout
package linear

import cats.*
import cats.syntax.all.given

import scala.math.Numeric.Implicits.given

def spaceBetweenElements[T: Numeric](starts: List[T], sizes: List[T]): List[T] =
  def helper(previousEnd: T, starts : List[(T, T)]) : List[T] =
    starts match
      case (start, size) :: others =>
        (start - previousEnd) :: helper(start + size, others)
      case Nil =>
        Nil
    end match
  end helper
  
  assume(starts.size == sizes.size && starts.nonEmpty)
  helper(starts.head + sizes.head, starts.tail.zip(sizes.tail))
end spaceBetweenElements

def spaceAroundElements[T: Numeric](starts: List[T], sizes: List[T], space : T): List[T] =
  val between = spaceBetweenElements(starts, sizes)
  val endGap = space - (sizes.last + starts.last)
  (starts.head :: between) :+ endGap
end spaceAroundElements

def minimalRequiredSpace[T: Numeric](widgets: List[T]): T = widgets.sum

def spaceCovered[T : Numeric](starts : List[T], sizes : List[T]) : CoveredSpace[T] =
  CoveredSpace(starts.head, starts.last + sizes.last)
end spaceCovered

def beginEndGaps[T : Numeric](starts : List[T], sizes : List[T], space : T) : (T, T) =
  val around = spaceAroundElements(starts, sizes, space)
  (around.head, around.last)
end beginEndGaps
