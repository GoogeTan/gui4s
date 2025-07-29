package me.katze.gui4s.example
package api.exported

import impl.containerPlacementCurriedOvergeneralized
import place.*

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.Monad
import me.katze.gui4s.example.{*, given}
import api.exported.given
import api.{LayoutPlacementMeta, given}

import cats.kernel.Monoid
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy, Widget, linearLayout}

import scala.language.experimental.namedTypeArguments

def skijaColumn[
  F[+_] : {Monad, ForeighFunctionInterface as ffi},
  Update[_] : Monad,
  Draw : Monoid,
  MeasurementUnit : Fractional,
  RecompositionReaction : Monoid,
  HandleableEvent,
  PlaceError,
](
  errors: ElementPlacementInInfiniteContainerAttemptError[PlaceError],
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
  mainAxisStrategy: MainAxisPlacementStrategy[MeasurementUnit],
  additionalAxisStrategy: AdditionalAxisPlacementStrategy,
  drawAt : (Draw, LayoutPlacementMeta[MeasurementUnit]) => Draw,
  updateAt : [T] => (Update[T], LayoutPlacementMeta[MeasurementUnit]) => Update[T],
  isEventConsumed : Update[Boolean]
) =
  skijaLayout[
    Update,
    SkijaPlaceInnerT[F, MeasurementUnit, PlaceError],
    Draw,
    MeasurementUnit,
    RecompositionReaction,
    HandleableEvent
  ](
    children,
    mainAxis,
    mainAxisStrategyPlacement(mainAxisStrategy, _, _, errors),
    additionalAxisStrategyPlacement(additionalAxisStrategy, _, _, errors),
    skijaGetBounds,
    skijaSetBounds,
    drawAt,
    updateAt,
    isEventConsumed
  )
end skijaColumn

def skijaLayout[
  Update[_] : Monad,
  OuterPlace[_] : Monad,
  Draw : Monoid,
  MeasurementUnit : Numeric,
  RecompositionReaction : Monoid,
  HandleableEvent
](
  children : List[OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]],
  mainAxis : Axis,
  mainAxisPlacement : (List[MeasurementUnit], AxisBounds[MeasurementUnit]) => OuterPlace[List[MeasurementUnit]],
  additionalAxisPlacement : (MeasurementUnit, AxisBounds[MeasurementUnit]) => OuterPlace[MeasurementUnit],
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
    layout = containerPlacementCurriedOvergeneralized(
      getBounds,
      setBounds,
      mainAxis,
      _,
      mainAxisPlacement,
      additionalAxisPlacement
    ),
    adjustDrawToMeta = drawAt,
    adjustUpdateToMeta = updateAt,
    isEventConsumed = isEventConsumed
  )
end skijaLayout