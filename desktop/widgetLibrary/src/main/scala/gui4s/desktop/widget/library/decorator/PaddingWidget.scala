package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all.*
import catnip.syntax.monad.MonadErrorC
import cats.syntax.all.*
import cats.{Applicative, Comonad, Functor, Id, ~>}
import gui4s.core.geometry.*
import gui4s.core.kit.widget.*
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.desktop.widget.library.decorator.Decorator
import gui4s.desktop.widget.library.decorator.Decorator.given

type PaddingWidget[Widget, Padding] = Padding => Decorator[Widget]

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
  gapPaddingWidget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent, Padding](
    paddings => placementDecorator[Update, OuterPlace * InnerPlace, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent](placementDecoration(paddings)),
    paddings => updateDecorator[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent](updateDecorations(paddings)),
    paddings => drawDecorator[Update, OuterPlace, InnerPlace, Draw, RecompositionReaction, HandleableEvent](drawDecoration(paddings))
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
  OuterPlace[_] : MonadErrorC[PlaceError],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Fractional as MUF,
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
      PlacementStrategy.PlaceIndependently(placementStrategy, MUF.zero),
      placementStrategy
    )
end paddingWidget
