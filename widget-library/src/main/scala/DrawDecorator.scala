package me.katze.gui4s.widget.library

import _root_.me.katze.gui4s.geometry.Rect
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.katze.gui4s.layout.{*, given}

def freeDrawDecorator[
  Update[_] : Monad as M,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit,
](
  original : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
  toDraw : (Draw, Rect[MeasurementUnit]) => Draw
) : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  basicDecoratorWithRect[
    Update,
    Place,
    Sized[MeasurementUnit, *],
    Draw,
    RecompositionReaction,
    HandleableEvent,
  ](
    "draw decorator",
    original,
    sizedWidget =>
      sizedWidget.mapValue(
        placedWidget =>
          drawDecorator(placedWidget.asWrapper, toDraw(_, sizedWidget.size))
      )
  )
end freeDrawDecorator

def drawDecorator[
  T,
  Update[_] : Monad as M,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  original : Widget.ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
  toDraw : Draw => Draw
) : Widget.ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
  original.copy(
    valueIsDrawable = value => toDraw(original.valueIsDrawable(value))
  )
end drawDecorator

