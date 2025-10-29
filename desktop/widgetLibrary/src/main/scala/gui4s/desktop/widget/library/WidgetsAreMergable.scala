package gui4s.desktop.widget.library

import catnip.syntax.additional.*
import catnip.syntax.applicative.*
import cats.syntax.all.*
import cats.{Comonad, Functor, Monad}
import gui4s.core.widget.merge.Mergable

def widgetsAreMergable[
  Update[_],
  OuterPlace[_] : Monad as OPM,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : Mergable[OuterPlace[InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]] =
  type Place[Value] = OuterPlace[InnerPlace[Value]]
  given Functor[Place] = nestedFunctorsAreFunctors[OuterPlace, InnerPlace]

  (path, oldWidget, newWidget) =>
    Monad[OuterPlace].flatMap2(
      oldWidget, newWidget
    )(
      (oldWithInnerPlace, nextWidgetToMerge) =>
        widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent](
          nextWidgetToMerge.extract,
          path,
          widgetHasInnerStates(oldWithInnerPlace.extract)
        )
    )
end widgetsAreMergable