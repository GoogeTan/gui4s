package me.katze.gui4s.example
package api

import catnip.syntax.all.{*, given}
import cats.{Functor, Monad}
import cats.syntax.all.*
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.Widget

def clipWidget[
  Update[_] : Monad as UM,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  Shape,
](
    withClip : [T] => (Shape, Update[T]) => Update[T],
    drawModifier : (Shape, Draw) => Draw
)(
    freeWidgetToClip : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
    shapeFabric : Rect[MeasurementUnit] => Shape
) : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  PF.map(
    freeWidgetToClip
  )(
    sizedWidget =>
      sizedWidget.mapValue(
        placedWidget =>
          val shape = shapeFabric(sizedWidget.size)
          def convert(widget : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]) =
            clipWidget[Update, Place, Draw, RecompositionReaction, HandleableEvent, MeasurementUnit, Shape](withClip, drawModifier)(widget, shapeFabric)

          println("clip created")
          placedWidget.copy[
            Update,
            Place * Sized[MeasurementUnit, *],
            Draw,
            RecompositionReaction,
            HandleableEvent
          ](
            asFree = convert(placedWidget.asFree),
            handleEvent = (path : Path, event : HandleableEvent) =>
                withClip[Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]](shape, placedWidget.handleEvent(path, event)).map(convert),
            draw = drawModifier(shape, placedWidget.draw),
            mergeWithOldState = (path, oldState) =>
              convert(placedWidget.mergeWithOldState(path, oldState))
          )
      )
  )
end clipWidget