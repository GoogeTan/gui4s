package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.Monad
import cats.syntax.all.*
import me.katze.gui4s.example.{*, given}
import api.exported.given
import api.{LayoutPlacementMeta, given}

import cats.kernel.Monoid
import me.katze.gui4s.geometry.Axis
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement, rowColumnLayoutPlacement}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{Widget, linearLayout}

import scala.language.experimental.namedTypeArguments

def skijaLayout[
  F[+_] : {Monad, ForeighFunctionInterface as ffi},
  Update[_] : Monad,
  Draw : Monoid,
  MeasurementUnit : Fractional,
  RecompositionReaction : Monoid,
  HandleableEvent,
  PlaceError,
](
  children : List[
    SkijaOuterPlace[
      F,
      MeasurementUnit,
      PlaceError,
      Sized[MeasurementUnit,
        Widget[
          Update,
          [Value] =>> SkijaOuterPlace[F, MeasurementUnit, PlaceError, Sized[MeasurementUnit, Value]],
          Draw,
          RecompositionReaction,
          HandleableEvent
        ]
      ]
    ]
  ],
  mainAxis : Axis,
  mainAxisPlacement : MainAxisPlacement[SkijaOuterPlaceT[F, MeasurementUnit, PlaceError], MeasurementUnit],
  additionalAxisPlacement : AdditionalAxisPlacement[SkijaOuterPlaceT[F, MeasurementUnit, PlaceError], MeasurementUnit],
  drawAt : (Draw, LayoutPlacementMeta[MeasurementUnit]) => Draw,
  updateAt : [T] => (Update[T], LayoutPlacementMeta[MeasurementUnit]) => Update[T],
  isEventConsumed : Update[Boolean]
) =
  placementAwareLayout[
    Update,
    SkijaOuterPlaceT[F, MeasurementUnit, PlaceError],
    Draw,
    MeasurementUnit,
    RecompositionReaction,
    HandleableEvent
  ](
    children,
    mainAxis,
    mainAxisPlacement,
    additionalAxisPlacement,
    SkijaOuterPlace.getBounds,
    SkijaOuterPlace.setBounds,
    drawAt,
    updateAt,
    isEventConsumed
  )
end skijaLayout

def placementAwareLayout[
  Update[_] : Monad,
  OuterPlace[_] : Monad,
  Draw : Monoid,
  MeasurementUnit : Numeric,
  RecompositionReaction : Monoid,
  HandleableEvent
](
  children : List[OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]],
  mainAxis : Axis,
  mainAxisPlacement : MainAxisPlacement[OuterPlace, MeasurementUnit],
  additionalAxisPlacement : AdditionalAxisPlacement[OuterPlace, MeasurementUnit],
  getBounds : OuterPlace[Bounds[MeasurementUnit]],
  setBounds : Bounds[MeasurementUnit] => OuterPlace[Unit],
  drawAt : (Draw, LayoutPlacementMeta[MeasurementUnit]) => Draw,
  updateAt : [T] => (Update[T], LayoutPlacementMeta[MeasurementUnit]) => Update[T],
  isEventConsumed : Update[Boolean]
) : OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  linearLayout[
    Update,
    OuterPlace * Sized[MeasurementUnit, *],
    Draw,
    RecompositionReaction,
    HandleableEvent,
    LayoutPlacementMeta[MeasurementUnit]
  ](
    children = children,
    layout = children => rowColumnLayoutPlacement(
      getBounds,
      setBounds,
      mainAxis,
      children,
      mainAxisPlacement,
      additionalAxisPlacement
    ).map(_.mapValue(placedChildrenAsChildrenWithMetadata)),
    adjustDrawToMeta = drawAt,
    adjustUpdateToMeta = updateAt,
    isEventConsumed = isEventConsumed
  )
end placementAwareLayout

def placedChildrenAsChildrenWithMetadata[MeasurementUnit, T](lst: List[Placed[MeasurementUnit, T]]): List[(T, LayoutPlacementMeta[MeasurementUnit])] =
  lst.map(placedElementAsLayoutMetadata)
end placedChildrenAsChildrenWithMetadata

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, LayoutPlacementMeta[MeasurementUnit]) =
  (placed.value, new LayoutPlacementMeta(placed))
end placedElementAsLayoutMetadata
