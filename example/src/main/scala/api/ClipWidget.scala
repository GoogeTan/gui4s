package me.katze.gui4s.example
package api

import catnip.syntax.all.{*, given}
import cats.{Functor, Monad}
import cats.syntax.all.*
import io.github.humbleui.skija.Path
import me.katze.gui4s.geometry.{Point2d, Point3d, Rect}
import me.katze.gui4s.widget.library.{ValueType, Widget, drawDecorator, eventHandleDecorator}
import me.katze.gui4s.layout.{*, given}

def clipWidget[
  Update[_] : Monad as UM,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  Shape,
](
    getCoordinates : Update[Point2d[MeasurementUnit]],
    withClip : [T] => (Shape, Update[T]) => Update[T],
    drawModifier : (Shape, Draw) => Draw
)(
    freeWidgetToClip : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
    shapeFabric : Rect[MeasurementUnit] => Shape
) : Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  freeWidgetToClip.map(
    sizedWidgetToClip =>
      sizedWidgetToClip.mapValue {
        case widgetToClip : Widget.ValueWrapper[t, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent] =>
          val shape = shapeFabric(sizedWidgetToClip.size)
          drawDecorator(
            eventHandleDecorator[
              t, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent
            ](using UM, summon)(
              widgetToClip,
              originalUpdate =>
                (self, path, event) =>
                  withClip(shape, originalUpdate(self, path, event))
            ),
            drawModifier(shape, _)
          )
      }
  )
end clipWidget