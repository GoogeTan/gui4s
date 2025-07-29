package me.katze.gui4s.widget.library

import cats.syntax.all.*
import cats.{Functor, Monad}
import _root_.me.katze.gui4s.geometry.Rect
import catnip.syntax.all.{*, given}
import me.katze.gui4s.layout.{*, given}

def drawDecorator[
  T,
  Update[_] : Monad as M,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit,
](
  original : Place[Sized[MeasurementUnit, Widget.ValueWrapper[T, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
  toDraw : (Draw, Rect[MeasurementUnit]) => Draw
) : Place[Sized[MeasurementUnit, Widget.ValueWrapper[T, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  original.map(
    sizedWidget =>
      sizedWidget.mapValue(
        widget =>
        widget.copy(
          valueIsDrawable = value => toDraw(widget.valueIsDrawable(value), sizedWidget.size)
        )
      )
  )
end drawDecorator
