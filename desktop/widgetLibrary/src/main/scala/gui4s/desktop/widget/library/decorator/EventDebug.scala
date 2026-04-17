package gui4s.desktop.widget.library.decorator

import cats.{Apply, Comonad, Functor}
import cats.syntax.all.*
import gui4s.desktop.widget.library.widgetHandlesEvent

def eventDebug[
  Update[_] : Apply as NUF,
  PlacementEffect[_] :Functor,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
](
  original : FreeWidgetWithSituated[
    Update, PlacementEffect, Situated, Draw, RecompositionReaction
  ],
  before : Update[Unit],
  after : Update[Unit],
) : FreeWidgetWithSituated[
  Update, PlacementEffect, Situated, Draw, RecompositionReaction
] =
  trueUpdateDecoratorWithRect(
    original,
    situatedWidget =>
      before *> widgetHandlesEvent(situatedWidget.extract) <* after
  )
end eventDebug
