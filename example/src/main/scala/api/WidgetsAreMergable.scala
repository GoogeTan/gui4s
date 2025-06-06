package me.katze.gui4s.example
package api

import cats.{Functor, Monad}
import cats.syntax.all.*
import catnip.syntax.all.{*, given}
import me.katze.gui4s.layout.{Placed, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.merge.Mergable

import scala.language.experimental.namedTypeArguments

def skijaWidgetsAreMergable[
  Update[+ _],
  SimplePlace[+ _]: Monad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit
] : Mergable[[Value] =>> SimplePlace[Placed[MeasurementUnit, Value]], SkijaWidget_[Update, [Value] =>> SimplePlace[Placed[MeasurementUnit, Value]], Draw, RecompositionReaction, HandleableEvent]] =
  type Widget = SkijaWidget_[Update, [Value] =>> SimplePlace[Placed[MeasurementUnit, Value]], Draw, RecompositionReaction, HandleableEvent]

  def helper(path : Path, current : Widget, tail : List[SimplePlace[Placed[MeasurementUnit, Widget]]]) : SimplePlace[Placed[MeasurementUnit, Widget]] =
    tail match
      case head :: next =>
          head.flatMap(headValue =>
            skijaWidgetMergesWithOldState[
              Place = [Value] =>> SimplePlace[Placed[MeasurementUnit, Value]]
            ](headValue.value, path, skijaWidgetHasInnerStates(current))
          ).flatMap((placedWidget : Placed[MeasurementUnit, Widget]) => helper(path, placedWidget.value, next))
      case Nil =>
        skijaWidgetAsFree[Place = [Value] =>> SimplePlace[Placed[MeasurementUnit, Value]]](current)
    end match
  end helper

  (path, old, news) => helper(path, old, news.toList)
end skijaWidgetsAreMergable