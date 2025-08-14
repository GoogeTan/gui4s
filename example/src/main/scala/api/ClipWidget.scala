package me.katze.gui4s.example
package api

import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{Widget, basicPlaceDecorator, drawDecorator, eventHandleDecorator}

class ClipF[
  Update[_] : Monad as UM,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  Shape
](
  withClip : [T] => (Shape, Update[T]) => Update[T],
  drawModifier : (Shape, Draw) => Draw,
  shapeFabric : Rect[MeasurementUnit] => Shape
) extends (Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]
    => Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]):
  override def apply(
                      v1: Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]]
                    ): Place[Sized[MeasurementUnit, Widget[Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent]]] =
    println("shape called")
    v1.map(sizedWidgetToClip =>
      sizedWidgetToClip.mapValue(
        widgetToClip =>
          val shape = shapeFabric(sizedWidgetToClip.size)
          println("shaped with " + sizedWidgetToClip.size.toString)
          drawDecorator(
            eventHandleDecorator[
              Update, Place * Sized[MeasurementUnit, *], Draw, RecompositionReaction, HandleableEvent
            ](using UM, summon)(
              widgetToClip,
              [T] => originalUpdate =>
                (self, path, event) =>
                  withClip(shape, originalUpdate(self, path, event))
            ).asWrapper,
            drawModifier(shape, _)
          )
      )
    )
  end apply

  override def toString(): String = "<clip function 1>"
end ClipF

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
  basicPlaceDecorator[
    Update,
    Place * Sized[MeasurementUnit, *],
    Draw,
    RecompositionReaction,
    HandleableEvent
  ](
    mark = "clip",
    original = freeWidgetToClip,
    f = ClipF[Update, Place, Draw, RecompositionReaction, HandleableEvent, MeasurementUnit, Shape](withClip, drawModifier, shapeFabric)
  )
end clipWidget