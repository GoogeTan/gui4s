package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all.*
import cats.{Comonad, Functor, Id, ~>}
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.PaddingWidget

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[
  Update[_] : Functor,
  PlacementEffect[_] : Functor,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Meta,
  Paddings,
](
  container : ContainerWidget[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent], Id, PlacementEffect * Situated, Meta],
  boundsWithPaddings : Paddings => PlacementEffect ~> PlacementEffect,
  innerPlaceWithPaddings : [T] => Paddings => Situated[T] => Situated[(T, Meta)],
): PaddingWidget[
  PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]],
  Paddings
] =
  gui4s.core.widget.library.decorator.gapPaddingWidget(
    container = container,
    boundsWithPaddings = boundsWithPaddings,
    innerPlaceWithPaddings = innerPlaceWithPaddings
  )
end gapPaddingWidget
