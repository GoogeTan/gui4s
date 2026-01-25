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
  PlacementEffect[_] : Functor,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Padding,
](
  placementDecoration : Padding => (PlacementEffect * Situated) ~> (PlacementEffect * Situated),
  updateDecorations : Padding => Decorator[WidgetHandlesEvent[HandleableEvent, Update[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]]]]],
  drawDecoration : Padding => Situated[Draw] => Draw
): PaddingWidget[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]], Padding] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]
  gui4s.core.widget.library.decorator.gapPaddingWidget(
    paddings => placementDecorator(placementDecoration(paddings)),
    paddings => updateDecorator[Place = (PlacementEffect * Situated)](updateDecorations(paddings)),
    paddings => drawDecorator(drawDecoration(paddings))
  )
end gapPaddingWidget
