package me.katze.gui4s.widget
package library

import catnip.BiMonad
import catnip.syntax.bimonad.given 
import cats.*
import cats.syntax.all.*
import me.katze.gui4s.widget
import me.katze.gui4s.widget.layout.{LayoutDraw, LayoutWidget}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{Widget, given}

type LayoutPlacement[Place[_], MeasurementUnit, PlacementMeta, Widget[_], Axis] =
  [Event] => (
      mainAxis               : Axis,
      children               : List[Place[Widget[Event]]],
      mainAxisStrategy       : MainAxisPlacementStrategy[MeasurementUnit],
      additionalAxisStrategy : AdditionalAxisPlacementStrategy
    ) => Place[List[(Widget[Event], PlacementMeta)]]

type LayoutPlacementGeneralized[Place[_], MeasurementUnit, PlacementMeta, Widget, Axis] =
  (
    mainAxis               : Axis,
    children               :  List[Place[Widget]],
    mainAxisStrategy       :  MainAxisPlacementStrategy[MeasurementUnit],
    additionalAxisStrategy : AdditionalAxisPlacementStrategy
  ) => Place[List[(Widget, PlacementMeta)]]

type LinearLayout[
  Widget[_],
  MeasurementUnit,
  Axis,
] = [Event] => (
  children               : List[Widget[Event]],
  mainAxis               : Axis,
  mainAxisStrategy       : MainAxisPlacementStrategy[MeasurementUnit],
  additionalAxisStrategy : AdditionalAxisPlacementStrategy,
) => Widget[Event]

def linearLayout[
  Update[+_, +_]: BiMonad,
  Draw : Monoid,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  MeasurementUnit,
  PlacementMeta,
  SystemEvent,
  Axis
](
    using LayoutDraw[Draw, PlacementMeta],
)(
  placement : LayoutPlacement[Place, MeasurementUnit, PlacementMeta, [Event] =>> Widget[[A] =>> Update[A, Event], Draw, Place, Recomposition, SystemEvent], Axis]
) : LinearLayout[[Event] =>> Place[Widget[[A] =>> Update[A, Event], Draw, Place, Recomposition, SystemEvent]], MeasurementUnit, Axis] =
  type Widget[+Event] = Place[widget.Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, SystemEvent]]
  
  def linearLayout_[Event](
                            children              : List[Widget[Event]],
                            axis                  : Axis,
                            mainAxisStrategy      : MainAxisPlacementStrategy[MeasurementUnit],
                            additionalAxisStrategy: AdditionalAxisPlacementStrategy,
                          ): Widget[Event] =
    placement(axis, children, mainAxisStrategy, additionalAxisStrategy)
      .map(
        LayoutWidget(
          _,
          linearLayout_(_, axis, mainAxisStrategy, additionalAxisStrategy))
      )

  [Event] => (
                children: List[Place[widget.Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, SystemEvent]]],
                mainAxis: Axis,
                mainAxisStrategy: MainAxisPlacementStrategy[MeasurementUnit],
                additionalAxisStrategy: AdditionalAxisPlacementStrategy,
  ) => linearLayout_(children, mainAxis, mainAxisStrategy, additionalAxisStrategy)
end linearLayout
