package me.katze.gui4s.widget.library

import _root_.me.katze.gui4s.geometry.Rect
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.katze.gui4s.layout.{*, given}

def freeDrawDecorator[
  Update[_] : Monad as M,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit,
](
  original : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
  toDraw : (Draw, Rect[MeasurementUnit]) => Draw
) : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  PF.map(
      original
  )(
    sizedWidget =>
      sizedWidget.mapValue(placedWidget =>
        def convert(widget : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]) =
          freeDrawDecorator(
            widget,
            toDraw
          )
        placedWidget.copy(
          asFree = convert(placedWidget.asFree),
          draw = toDraw(
            placedWidget.draw,
            sizedWidget.size
          ),
          handleEvent = (path, event) => placedWidget.handleEvent(path, event).map(convert),
          mergeWithOldState = (path, state) => convert(placedWidget.mergeWithOldState(path, state))
        )
      )
  )
end freeDrawDecorator