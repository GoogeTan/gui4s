package me.katze.gui4s.example
package api.impl

import api.*
import draw.given

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget
import me.katze.gui4s.widget.Widget
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{*, given}

type LayoutPlacementGeneralized2[Place[_], MeasurementUnit, PlacementMeta, Widget[_]] =
  [Event] => (Axis, List[Place[Widget[Event]]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy) => Place[List[(Widget[Event], PlacementMeta)]]

type LayoutPlacementGeneralized[Place[_], MeasurementUnit, PlacementMeta, Widget] =
   (Axis, List[Place[Widget]], MainAxisPlacementStrategy[MeasurementUnit], AdditionalAxisPlacementStrategy) => Place[List[(Widget, PlacementMeta)]]

final class LayoutApi_[
  Update[+_, +_]: BiMonad,
  Draw : Monoid,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  MeasurementUnit,
  PlacementMeta,
  SystemEvent,
](
    using LayoutDraw[Draw, PlacementMeta],
)(
  val placement : LayoutPlacementGeneralized2[Place, MeasurementUnit, PlacementMeta, [Event] =>> Widget[[A] =>> Update[A, Event], Draw, Place, Recomposition, SystemEvent]]
) extends LayoutApi[[Event] =>> Place[widget.Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, SystemEvent]], MeasurementUnit]:

  private type Widget[+Event] = Place[widget.Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, SystemEvent]]

  override def column[Event](
                              children          : List[Widget[Event]],
                              verticalStrategy  : MainAxisPlacementStrategy[MeasurementUnit],
                              horizontalStrategy: AdditionalAxisPlacementStrategy
                            ): Widget[Event] =
    linearLayout(
      children = children,
      axis = Axis.Vertical,
      mainAxisStrategy = verticalStrategy,
      additionalAxisStrategy = horizontalStrategy,
    )
  end column

  override def row[Event](
                            children          : List[Widget[Event]],
                            horizontalStrategy: MainAxisPlacementStrategy[MeasurementUnit],
                            verticalStrategy  : AdditionalAxisPlacementStrategy
                          ): Widget[Event] =
    linearLayout(
      children = children,
      axis = Axis.Horizontal,
      mainAxisStrategy = horizontalStrategy,
      additionalAxisStrategy = verticalStrategy,
    )
  end row

  def linearLayout[Event](
                                    children              : List[Widget[Event]],
                                    axis                  : Axis,
                                    mainAxisStrategy      : MainAxisPlacementStrategy[MeasurementUnit],
                                    additionalAxisStrategy: AdditionalAxisPlacementStrategy,
                                  ): Widget[Event] =
    layoutWidget[[W] =>> Update[W, Event], Draw, Place, Recomposition, PlacementMeta, SystemEvent](children, placement[Event](axis, _, mainAxisStrategy, additionalAxisStrategy))
  end linearLayout
end LayoutApi_

  
