package me.katze.gui4s.example
package api.decorator

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.example.api.*
import me.katze.gui4s.layout.{Point3d, Rect, Sized}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.{Widget, Widget_}

import scala.math.Numeric.Implicits.*
import scala.math.Ordered.orderingToOrdered

final case class ClickedAt[MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit):
  def isIn(rect : Rect[MeasurementUnit])(using n : Numeric[MeasurementUnit]) : Boolean =
    n.zero <= x && x <= rect.width 
      && n.zero <= y && y <= rect.height
end ClickedAt

def handleClick[
  Update[_] : Monad,
  OuterPlace[_] : Functor,
  Draw : Monoid,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric
](
   markEventHandled : Update[Unit],
   coordinatesOfTheWidget : Update[Point3d[MeasurementUnit]],
   differentiateEvent : HandleableEvent => Option[ClickedAt[MeasurementUnit]],
)(
  original : OuterPlace[Sized[MeasurementUnit, Widget_[Update, [Value] =>> OuterPlace[Sized[MeasurementUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]],
)(
  onClick : (Path, ClickedAt[MeasurementUnit]) => Update[Unit]
) : OuterPlace[Sized[MeasurementUnit, Widget_[Update,  [Value] =>> OuterPlace[Sized[MeasurementUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]] =
  eventCatcherWithWidgetsRect[Update, OuterPlace, Draw, RecompositionReaction, HandleableEvent, MeasurementUnit](markEventHandled, coordinatesOfTheWidget)(original):
    (path, rect, _, event) =>
      differentiateEvent(event) match
        case Some(value) if value.isIn(rect) =>
          onClick(path, value) *> true.pure[Update]
        case _ => false.pure[Update]
end handleClick
