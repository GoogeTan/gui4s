package me.katze.gui4s.example
package place

import api.impl.LayoutPlacementMeta
import api.{AdditionalAxisStrategy, MainAxisStrategy}

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.layout
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds}
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.{Placed, Sized}

import scala.math.Fractional.Implicits.{*, given}

def mainAxisStrategyPlacement[MU : Fractional](strategy: MainAxisStrategy[MU], elements: List[MU], bounds: AxisBounds[MU]): List[MU] =
  strategy match
    case MainAxisStrategy.Begin(gap) => placeBeginMany(elements.map(_ + gap))
    case MainAxisStrategy.Center(gap) => placeCenterMany(elements.map(_ + gap), bounds.max.getOrElse(throw Exception("TODO")))
    case MainAxisStrategy.End(gap) => placeEndMany(elements.map(_ + gap), bounds.max.getOrElse(throw Exception("TODO")))
    case MainAxisStrategy.SpaceBetween => placeSpaceBetween(elements, bounds.max.getOrElse(throw Exception("TODO")))
    case MainAxisStrategy.SpaceAround => placeSpaceAround(elements, bounds.max.getOrElse(throw Exception("TODO")))
  end match
end mainAxisStrategyPlacement

def additionalAxisStrategyPlacement[MU : Fractional](strategy: AdditionalAxisStrategy, element: MU, bounds: AxisBounds[MU]): MU =
  strategy match
    case AdditionalAxisStrategy.Begin => placeBegin
    case AdditionalAxisStrategy.Center => placeCenter(element, bounds.max.getOrElse(throw Exception("TODO")))
    case AdditionalAxisStrategy.End => placeEnd(element, bounds.max.getOrElse(throw Exception("TODO")))
  end match
end additionalAxisStrategyPlacement

def unpack[MU, T](lst: List[Placed[MU, T]]): List[(T, LayoutPlacementMeta[MU])] =
  lst.map(elem => (elem.value, LayoutPlacementMeta(elem.x, elem.y)))
end unpack

def rowColumnPlace[MU, T](
                            elements           : List[Sized[MU, T]],
                            bounds             : AxisDependentBounds[MU],
                            mainAxisPlace      : (List[MU], AxisBounds[MU]) => List[MU],
                            additionalAxisPlace: (MU, AxisBounds[MU]) => MU,
                          ): List[Placed[MU, T]] =
  val ys = mainAxisPlace(elements.map(_.mainAxisValue(bounds.axis)), bounds.mainAxis)
  val xs = elements.map(el => additionalAxisPlace(el.additionalAxisValue(bounds.axis), bounds.additionalAxis))
  Monad[List].map3(elements, xs, ys)((value, x, y) => Placed(value.value, x, y, value.width, value.height))
end rowColumnPlace