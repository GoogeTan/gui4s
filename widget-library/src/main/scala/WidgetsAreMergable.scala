package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import catnip.syntax.applicative.given
import cats.syntax.all.*
import cats.{Comonad, Functor, Monad}
import me.katze.gui4s.widget.merge.Mergable

import scala.language.experimental.namedTypeArguments

def widgetsAreMergable[
  Update[_],
  OuterPlace[_] : Monad,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : Mergable[OuterPlace[InnerPlace[Widget_[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]] =
  type Place[Value] = OuterPlace[InnerPlace[Value]]
  given Functor[Place] = nestedFunctorsAreFunctors

  (path, oldWidget, newWidget) =>
    Monad[OuterPlace].flatMap2(
      oldWidget, newWidget
    )(
      (oldWithInnerPlace, nextWidgetToMerge) =>
        widgetMergesWithOldState[Place = Place](
          nextWidgetToMerge.extract,
          path,
          widgetHasInnerStates[Place = Place](oldWithInnerPlace.extract)
        )
    )
end widgetsAreMergable