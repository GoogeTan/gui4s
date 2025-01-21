package me.katze.gui4s.example
package place

import api.impl.LayoutPlacementMeta
import api.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.layout
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds}
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.{Placed, Sized}

import scala.math.Fractional.Implicits.{*, given}

def mainAxisStrategyPlacement[MU : Fractional](strategy: MainAxisPlacementStrategy[MU], elements: List[MU], space : => MU): List[MU] =
  (
    strategy match
      case MainAxisPlacementStrategy.Begin(gap) => placeBeginMany(elements.map(_ + gap))
      case MainAxisPlacementStrategy.Center(gap) => placeCenterMany(elements.map(_ + gap), space)
      case MainAxisPlacementStrategy.End(gap) => placeEndMany(elements.map(_ + gap), space)
      case MainAxisPlacementStrategy.SpaceBetween => placeSpaceBetween(elements, space)
      case MainAxisPlacementStrategy.SpaceAround => placeSpaceAround(elements, space)
  ).map(_.coordinateOfStart)
end mainAxisStrategyPlacement

def additionalAxisStrategyPlacement[MU : Fractional](strategy: AdditionalAxisPlacementStrategy, element: MU, space : => MU): MU =
  strategy match
    case AdditionalAxisPlacementStrategy.Begin => placeBegin
    case AdditionalAxisPlacementStrategy.Center => placeCenter(element, space)
    case AdditionalAxisPlacementStrategy.End => placeEnd(element, space)
  end match
end additionalAxisStrategyPlacement

//RENAMEME
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

  Monad[List].map(xs.zip(ys).zip(elements))((coords, value) => Placed(value.value, coords._1, coords._2, value.width, value.height))
end rowColumnPlace