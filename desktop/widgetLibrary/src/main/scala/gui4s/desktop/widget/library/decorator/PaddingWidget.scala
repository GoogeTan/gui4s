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
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Padding,
](
  placementDecoration : Padding => (OuterPlace * InnerPlace) ~> (OuterPlace * InnerPlace),
  updateDecorations : Padding => Decorator[WidgetHandlesEvent[HandleableEvent, Update[OuterPlace[InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]]]],
  drawDecoration : Padding => InnerPlace[Draw] => Draw
): PaddingWidget[OuterPlace[InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]], Padding] =
  given Functor[OuterPlace * InnerPlace] = nestedFunctorsAreFunctors[OuterPlace, InnerPlace]
  gui4s.core.widget.library.decorator.gapPaddingWidget(
    paddings => placementDecorator(placementDecoration(paddings)),
    paddings => updateDecorator[Place = (OuterPlace * InnerPlace)](updateDecorations(paddings)),
    paddings => drawDecorator(drawDecoration(paddings))
  )
end gapPaddingWidget
