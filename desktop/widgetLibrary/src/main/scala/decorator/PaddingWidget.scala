package gui4s.decktop.widget.library
package decorator

import decorator.Decorator
import decorator.Decorator.given

import catnip.syntax.additional.*
import catnip.syntax.all.given
import catnip.syntax.monad.MonadErrorT
import cats.syntax.all.*
import cats.{Applicative, Comonad, Functor, Id}
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy}
import gui4s.decktop.widget.library.{LinearContainer, Widget, WidgetHandlesEvent}

type PaddingWidget[Widget, Padding] = Padding => Decorator[Widget]

def gapPaddingWidget[
  Update[_] : Functor,
  Place[_] : Functor,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Padding,
](
  placementDecoration : Padding => [T] => Place[InnerPlace[T]] => Place[InnerPlace[T]],
  updateDecorations : Padding => Decorator[WidgetHandlesEvent[HandleableEvent, Update[Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]]]],
  drawDecoration : Padding => InnerPlace[Draw] => Draw
): PaddingWidget[Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]], Padding] =
  gapPaddingWidget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent, Padding](
    paddings => placementDecorator[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent](placementDecoration(paddings)),
    paddings => updateDecorator[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent](updateDecorations(paddings)),
    paddings => drawDecorator[Update, Place, InnerPlace, Draw, RecompositionReaction, HandleableEvent](drawDecoration(paddings))
  )
end gapPaddingWidget

def gapPaddingWidget[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Padding,
](
  placementDecorator   : Padding => Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
  eventHandleDecorator : Padding => Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
  drawDecorator        : Padding => Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
) : PaddingWidget[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]], Padding] =
  paddings =>
    placementDecorator(paddings) |+| eventHandleDecorator(paddings) |+| drawDecorator(paddings)
end gapPaddingWidget

def paddingLayoutPlacementStrategy[
  Place[_] : Applicative,
  MeasurementUnit: Fractional,
](
    paddings: Paddings[Padding[MeasurementUnit]],
): OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit] =
  (paddings.left, paddings.right) match
    case (Padding.Gap(_), _) => OneElementPlacementStrategy.Begin[Place, MeasurementUnit, MeasurementUnit]
    case (Padding.Fill, Padding.Gap(_)) => OneElementPlacementStrategy.End
    case (Padding.Fill, Padding.Fill) => OneElementPlacementStrategy.Center
end paddingLayoutPlacementStrategy

def paddingWidget[
  Update[_],
  OuterPlace[_] : MonadErrorT[PlaceError],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Fractional,
  PlaceError
](
  innerGaps : PaddingWidget[
    Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    Paddings[MeasurementUnit]
  ],
  layout : LinearContainer[
    Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    OuterPlace,
    Id,
    InfinityOr[MeasurementUnit],
    MeasurementUnit,
    Axis
  ],
  infinitePaddingInInfiniteContainer : PlaceError
) : PaddingWidget[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  Paddings[Padding[MeasurementUnit]]
] =
  paddings => widget =>
    val placementStrategy = OneElementPlacementStrategy.ErrorIfInfinity(
      paddingLayoutPlacementStrategy(paddings),
      infinitePaddingInInfiniteContainer
    )
    layout(       
      innerGaps(paddings.map(_.gapOrZero))(widget),
      Axis.Vertical,
      ManyElementsPlacementStrategy.OneByOne(placementStrategy),
      placementStrategy
    )
end paddingWidget