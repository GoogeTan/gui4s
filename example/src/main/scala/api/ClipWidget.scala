package me.katze.gui4s.example
package api

import catnip.syntax.all.{*, given}
import cats.{Functor, Monad}
import cats.syntax.all.*
import io.github.humbleui.skija.Path
import me.katze.gui4s.geometry.{Point2d, Point3d, Rect}
import me.katze.gui4s.widget.library.{Widget, drawDecorator, eventHandleDecorator}
import me.katze.gui4s.layout.{*, given}

def clipWidget[
  Update[_] : Monad as UM,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  Shape,
  T
](
    getCoordinates : Update[Point2d[MeasurementUnit]],
    pointOfTheEvent : HandleableEvent => Option[Point2d[MeasurementUnit]],
    testPointInsideOfShape : (Shape, Point2d[MeasurementUnit]) => Update[Boolean],
    drawModifier : (Draw, Shape) => Draw
)(
    freeWidgetToClip : Place[Sized[MeasurementUnit, Widget.ValueWrapper[T, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]],
    shapeFabric : Rect[MeasurementUnit] => Shape
) : Place[Sized[MeasurementUnit, Widget.ValueWrapper[T, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
  freeWidgetToClip.map(
    sizedWidgetToClip =>
      sizedWidgetToClip.mapValue(
        widgetToClip =>
          val shape = shapeFabric(sizedWidgetToClip.size)
          drawDecorator(
            eventHandleDecorator[
              T, Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent
            ](using UM, summon)(
              widgetToClip,
              originalUpdate =>
                (self, path, event) =>
                  pointOfTheEvent(event) match
                    case Some(value) =>
                      getCoordinates.map(_ - value).flatMap(testPointInsideOfShape(shape, _)).ifM(
                        ifTrue = originalUpdate(self, path, event),
                        ifFalse = widgetToClip.valueAsFree(self).pure[Update]
                      )
                    case None =>
                      originalUpdate(self, path, event)
            ),
            drawModifier(_, shape)
          )
      )
  )
end clipWidget