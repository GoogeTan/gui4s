package me.katze.gui4s.example
package api.decorator

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.example.api.*
import me.katze.gui4s.layout.{Sized, Placed, Rect}
import me.katze.gui4s.widget.Path

import scala.math.Numeric.Implicits.*
import scala.math.Ordered.orderingToOrdered

final case class ClickedAt[MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit):
  def isIn(rect : Rect[MeasurementUnit])(using n : Numeric[MeasurementUnit]) : Boolean =
    n.zero <= x && x <= rect.width 
      && n.zero <= y && y <= rect.height
end ClickedAt

def handleClick[
  T,
  Update[+_] : Monad,
  SimplePlace[+_] : Functor,
  Draw : Monoid,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric
](
   original : SimplePlace[Placed[MeasurementUnit, SkijaWidget[T, Update, [Value] =>> SimplePlace[Placed[MeasurementUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]],
   markEventHandled : Update[Unit],
   differentiateEvent : HandleableEvent => Option[ClickedAt[MeasurementUnit]],
   onClick : (Path, ClickedAt[MeasurementUnit]) => Update[Unit]
) : SimplePlace[Placed[MeasurementUnit, SkijaWidget[T, Update,  [Value] =>> SimplePlace[Placed[MeasurementUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]] =
  eventCatcherWithWidgetsRect[T, Update, SimplePlace, Draw, RecompositionReaction, HandleableEvent, MeasurementUnit](markEventHandled)(original):
    (path, rect, _, event) =>
      differentiateEvent(event) match
        case Some(value) if value.isIn(rect) =>
          onClick(path, value) *> true.pure[Update]
        case _ => false.pure[Update]
end handleClick
