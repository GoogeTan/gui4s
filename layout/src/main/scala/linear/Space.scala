package me.katze.gui4s.layout
package linear

import cats.*
import cats.syntax.all.{*, given}
import scala.math.Numeric.Implicits.given

// TODO Сделать за линейное время
def spaceBetweenElements[T: Numeric](starts: List[T], sizes: List[T]): List[T] =
  for
    i <- (1 until starts.length).toList
  yield starts(i) - starts(i - 1) - sizes(i - 1)
end spaceBetweenElements

def spaceAroundElements[T: Numeric](starts: List[T], sizes: List[T], space : T): List[T] =
  val between = spaceBetweenElements(starts, sizes)
  starts.head :: between ++ List(space - sizes.last - starts.last)
end spaceAroundElements

def minimalRequiredSpace[T: Numeric](widgets: List[T]): T = widgets.sum

def spaceCovered[T : Numeric](starts : List[T], sizes : List[T]) : CoveredSpace[T] =
  CoveredSpace(starts.head, starts.last + sizes.last)
end spaceCovered

def beginEndGaps[T : Numeric](starts : List[T], sizes : List[T], space : T) : (T, T) =
  val around = spaceAroundElements(starts, sizes, space)
  (around.head, around.last)
end beginEndGaps
