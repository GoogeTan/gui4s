package gui4s.core.widget.library.decorator

import cats.*
import cats.syntax.all.*
import Decorator.given
import catnip.syntax.monad.MonadErrorC
import gui4s.core.geometry.{Axis, InfinityOr}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.core.widget.library.LinearContainer

type PaddingWidget[Widget, Padding] = Padding => Decorator[Widget]

def gapPaddingWidget[
  Widget,
  Padding,
](
   placementDecorator   : Padding => Decorator[Widget],
   eventHandleDecorator : Padding => Decorator[Widget],
   drawDecorator        : Padding => Decorator[Widget],
 ) : PaddingWidget[Widget, Padding] =
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
  Widget,
  OuterPlace[_] : MonadErrorC[PlaceError],
  Place[_],
  MeasurementUnit : Fractional as MUF,
  PlaceError
](
   innerGaps : PaddingWidget[
     Place[Widget],
     Paddings[MeasurementUnit]
   ],
   layout : LinearContainer[
     Place[Widget],
     OuterPlace,
     Id,
     InfinityOr[MeasurementUnit],
     MeasurementUnit,
     Axis
   ],
   infinitePaddingInInfiniteContainer : PlaceError
 ) : PaddingWidget[
  Place[Widget],
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
