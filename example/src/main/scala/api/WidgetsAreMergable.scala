package me.katze.gui4s.example
package api

import cats.{Comonad, Functor, Monad}
import cats.syntax.all.*
import catnip.syntax.all.{*, given}
import me.katze.gui4s.layout.{Placed, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.merge.Mergable

import scala.language.experimental.namedTypeArguments

def skijaWidgetsAreMergable[
  Update[+ _],
  SimplePlace[+ _]: Monad,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : Mergable[[Value] =>> SimplePlace[InnerPlace[Value]], SkijaWidget_[Update, [Value] =>> SimplePlace[InnerPlace[Value]], Draw, RecompositionReaction, HandleableEvent]] =
  type Widget = SkijaWidget_[Update, [Value] =>> SimplePlace[InnerPlace[Value]], Draw, RecompositionReaction, HandleableEvent]

  def helper(path : Path, current : Widget, tail : List[SimplePlace[InnerPlace[Widget]]]) : SimplePlace[InnerPlace[Widget]] =
    tail match
      case head :: next =>
          head.flatMap(headValue =>
            skijaWidgetMergesWithOldState[
              Place = [Value] =>> SimplePlace[InnerPlace[Value]]
            ](headValue.extract, path, skijaWidgetHasInnerStates[
            Place = [Value] =>> SimplePlace[InnerPlace[Value]]
          ](current))
          ).flatMap((placedWidget : InnerPlace[Widget]) => helper(path, placedWidget.extract, next))
      case Nil =>
        skijaWidgetAsFree[Place = [Value] =>> SimplePlace[InnerPlace[Value]]](current)
    end match
  end helper

  (path, old, news) => helper(path, old, news.toList)
end skijaWidgetsAreMergable