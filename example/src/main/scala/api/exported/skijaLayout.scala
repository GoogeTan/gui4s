package me.katze.gui4s.example
package api.exported

import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement, rowColumnLayoutPlacement}
import place.*

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.Monad
import cats.syntax.all.*
import me.katze.gui4s.example.{*, given}
import api.exported.given
import api.{LayoutPlacementMeta, given}

import cats.kernel.Monoid
import me.katze.gui4s.geometry.Axis
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy, Widget, linearLayout}

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
    SkijaPlaceInner[
      F,
      MeasurementUnit,
      PlaceError,
      Sized[MeasurementUnit,
        Widget[
          Update,
          [Value] =>> SkijaPlaceInner[F, MeasurementUnit, PlaceError, Sized[MeasurementUnit, Value]],
          Draw,
          RecompositionReaction,
          HandleableEvent
        ]
      ]
    ]
  ],
  mainAxis : Axis,
  mainAxisPlacement : MainAxisPlacement[SkijaPlaceInnerT[F, MeasurementUnit, PlaceError], MeasurementUnit],
  additionalAxisPlacement : AdditionalAxisPlacement[SkijaPlaceInnerT[F, MeasurementUnit, PlaceError], MeasurementUnit],
  drawAt : (Draw, LayoutPlacementMeta[MeasurementUnit]) => Draw,
  updateAt : [T] => (Update[T], LayoutPlacementMeta[MeasurementUnit]) => Update[T],
  isEventConsumed : Update[Boolean]
) =
  placementAwareLayout[
    Update,
    SkijaPlaceInnerT[F, MeasurementUnit, PlaceError],
    Draw,
    MeasurementUnit,
    RecompositionReaction,
    HandleableEvent
  ](
    children,
    mainAxis,
    mainAxisPlacement,
    additionalAxisPlacement,
    skijaGetBounds,
    skijaSetBounds,
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
    ).map(_.mapValue(unpack)),
    adjustDrawToMeta = drawAt,
    adjustUpdateToMeta = updateAt,
    isEventConsumed = isEventConsumed
  )
end placementAwareLayout
