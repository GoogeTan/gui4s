package gui4s.core.widget.library.decorator

import catnip.syntax.all.{*, given}
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.{InfinityOr, Point2d, Rect}
import gui4s.core.layout.{ContainerStrategy, OneElementPlacementStrategy, PlacementStrategy, Sized}
import gui4s.core.widget.library.ContainerWidget

import scala.math.Fractional.Implicits.*

type PaddingWidget[Widget, Padding] = Padding => Decorator[Widget]

def paddingLayoutPlacementStrategy[
  Place[_] : Applicative,
  MeasurementUnit: Fractional as MUF,
](
   left : InfinityOr[MeasurementUnit],
   right : InfinityOr[MeasurementUnit],
   onInfinity : Place[Sized[MeasurementUnit, MeasurementUnit]]
): OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, InfinityOr[MeasurementUnit], MeasurementUnit] =
  (element, availableSizeOrInfinity) =>
    (left, right, availableSizeOrInfinity) match//TODO это не верно, так как оно, не займет всё место.
      case (InfinityOr(Some(l)), InfinityOr(Some(r)), _) =>
        Sized(
          value = l,
          size = l + element + r
        ).pure[Place]
      case (InfinityOr(Some(l)), InfinityOr(None), InfinityOr(Some(availableSize))) =>
        Sized(
          value = l,
          size = availableSize
        ).pure[Place]
      case (InfinityOr(None), InfinityOr(Some(r)), InfinityOr(Some(availableSizeOrInfinity))) =>
        Sized(
          value = availableSizeOrInfinity - element - r,
          size = availableSizeOrInfinity
        ).pure[Place]
      case (InfinityOr(None), InfinityOr(None), InfinityOr(Some(availableSizeOrInfinity))) =>
        Sized(
          value = (availableSizeOrInfinity - element) / MUF.fromInt(2),
          size = availableSizeOrInfinity
        ).pure[Place]
      case _ =>
        onInfinity
end paddingLayoutPlacementStrategy

/**
 * Одноместный контейнер, добавляющий отступы вокруг виджета.
 */
def paddingWidget[
  Widget,
  MeasuredWidget,
  PositionedWidget,
  PlacementEffect[_] : MonadErrorC[PlaceError] as ME,
  MeasurementUnit : Fractional as MUF,
  PlaceError,
](
  container : ContainerWidget[
    Widget,
    Widget,
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[MeasuredWidget],
      Rect[MeasurementUnit],
      Rect[InfinityOr[MeasurementUnit]],
      Id,
      PositionedWidget
    ]
  ],
  boundsWithPaddings : PlacementEffect ~> PlacementEffect,
  makeMeta : (MeasuredWidget, Point2d[MeasurementUnit]) => PositionedWidget,
  sizeOfItem : MeasuredWidget => Rect[MeasurementUnit],
  infinitePaddingInInfiniteContainer : => PlaceError,
  widget: Widget,
  paddings : Paddings[InfinityOr[MeasurementUnit]]
 ) : Widget =
  lazy val error = ME.raiseError[Sized[MeasurementUnit, MeasurementUnit]](infinitePaddingInInfiniteContainer)
  container(
    widget,
    ContainerStrategy.combine(
      measurementStrategy = boundsWithPaddings(_),
      placementStrategy = PlacementStrategy.Zip(
        paddingLayoutPlacementStrategy(paddings.left, paddings.right, error),
        paddingLayoutPlacementStrategy(paddings.top, paddings.bottom, error),
      ),
      someMap = makeMeta(_, _),
      sizeOfItem = sizeOfItem
    )
  )
end paddingWidget
