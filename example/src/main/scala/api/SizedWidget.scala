package me.katze.gui4s.example
package api

import api.exported.{*, given}

import cats.Monad
import cats.syntax.all.*
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget.library.Widget

type SizedWidget[Widget, MeasurementUnit] = (Widget, Rect[MeasurementUnit]) => Widget

def sizedWidget[
  F[_] : Monad,
  Update[_] : Monad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  PlaceError,
  T
] : SizedWidget[
  SkijaPlace[F, MeasurementUnit, PlaceError, Widget.ValueWrapper[T, Update, SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]],
  MeasurementUnit
] =
  (originalWidget, bounds) =>
    SkijaOuterPlace.withBounds(originalWidget, _ => new Bounds(bounds)).map(_.withSize(bounds))
end sizedWidget