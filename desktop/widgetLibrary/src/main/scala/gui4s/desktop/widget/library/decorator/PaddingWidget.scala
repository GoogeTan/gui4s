package gui4s.desktop.widget.library
package decorator

import scala.language.experimental.namedTypeArguments

import catnip.syntax.all._
import cats.Comonad
import cats.Functor
import cats.~>

import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.PaddingWidget

//TODO заменить на одноместный контейнер. Он почему-то уже используется в desktop.kit. Надо просто обобщить.
def gapPaddingWidget[
  Update[_] : Functor,
  OuterPlace[_] : Functor,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Padding,
](
  placementDecoration : Padding => (OuterPlace * Situated) ~> (OuterPlace * Situated),
  updateDecorations : Padding => Decorator[WidgetHandlesEvent[HandleableEvent, Update[OuterPlace[Situated[Widget[Update, OuterPlace * Situated, Draw, RecompositionReaction, HandleableEvent]]]]]],
  drawDecoration : Padding => Situated[Draw] => Draw
): PaddingWidget[OuterPlace[Situated[Widget[Update, OuterPlace * Situated, Draw, RecompositionReaction, HandleableEvent]]], Padding] =
  given Functor[OuterPlace * Situated] = nestedFunctorsAreFunctors[OuterPlace, Situated]
  gui4s.core.widget.library.decorator.gapPaddingWidget(
    paddings => placementDecorator(placementDecoration(paddings)),
    paddings => updateDecorator[Place = (OuterPlace * Situated)](updateDecorations(paddings)),
    paddings => drawDecorator(drawDecoration(paddings))
  )
end gapPaddingWidget
