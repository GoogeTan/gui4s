package gui4s.core.widget.library.decorator

import catnip.syntax.all.*
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, InfinityOr}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.core.widget.library.{ContainerWidget, LinearContainer}

type PaddingWidget[Widget, Padding] = Padding => Decorator[Widget]

/**
 * Одноместный контейнер, добавляющий отступы фиксированной длины вокруг виджета.
 */
def gapPaddingWidget[
  PlacedWidget,
  OuterPlace[_] : Functor,
  InnerPlace[_],
  Meta,
  MeasurementUnit,
  Paddings,
](
  container : ContainerWidget[PlacedWidget, Id, OuterPlace * InnerPlace, Meta],
  boundsWithPaddings : Paddings => OuterPlace ~> OuterPlace,
  innerPlaceWithPaddings : [T] => Paddings => InnerPlace[T] => InnerPlace[(T, Meta)],
): PaddingWidget[OuterPlace[InnerPlace[PlacedWidget]], Paddings] =
  paddings => original =>
    container(
      original,
      child =>
        boundsWithPaddings(paddings)(child).map(innerPlaceWithPaddings(paddings))
    )
end gapPaddingWidget

def paddingLayoutPlacementStrategy[
  Place[_] : Applicative,
  MeasurementUnit: Fractional,
](
   paddings: Paddings[Padding[MeasurementUnit]],
 ): OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit] =
  (paddings.left, paddings.right) match
    case (Padding.Gap(_), _) => OneElementPlacementStrategy.Begin[Place, MeasurementUnit, MeasurementUnit]
    case (Padding.Fill, Padding.Gap(_)) => OneElementPlacementStrategy.End
    case (Padding.Fill, Padding.Fill) => OneElementPlacementStrategy.Center
end paddingLayoutPlacementStrategy

def paddingWidget[
  Widget,
  PlacementEffect[_] : MonadErrorC[PlaceError],
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
     PlacementEffect,
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
