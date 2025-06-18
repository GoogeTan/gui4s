package me.katze.gui4s.widget.library

import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Applicative, Comonad, Functor, Monad}
import me.katze.gui4s.layout.{Placed, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.merge.Mergable

import scala.language.experimental.namedTypeArguments

// TODO refactor me
def skijaWidgetsAreMergable[
  Update[+ _],
  OuterPlace[+ _] : Functor,
  InnerPlace[+_] : Comonad,
  Merge[+_] : Applicative,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](using M : Monad[Merge * OuterPlace]) : Mergable[Merge * OuterPlace * InnerPlace, Widget_[Update, OuterPlace * InnerPlace, Merge * OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]] =
  type Place[Value] = OuterPlace[InnerPlace[Value]]
  type Widget = Widget_[Update, Place, Merge * Place, Draw, RecompositionReaction, HandleableEvent]
  given Functor[Merge * OuterPlace] = M

  def helper(path : Path, current : Widget, tail : List[Merge[OuterPlace[InnerPlace[Widget]]]]) : Merge[OuterPlace[InnerPlace[Widget]]] =
    tail match
      case headM :: nextM =>
        M.flatMap(
          M.flatMap(headM)(
            headValue =>
              widgetMergesWithOldState[Place = Place, Merge = Merge * Place](
                headValue.extract,
                path,
                widgetHasInnerStates[Place = Place](current)
              )
          )
        )(headMerged =>
          helper(path, headMerged.extract, nextM)
        )
      case Nil =>
        widgetAsFree[Place = Place](current).pure[Merge]
    end match
  end helper

  (path, old, news) => helper(path, old, news.toList)
end skijaWidgetsAreMergable