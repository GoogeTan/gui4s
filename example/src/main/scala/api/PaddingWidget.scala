package me.katze.gui4s.example.api

import catnip.syntax.all.given
import cats.Monad
import cats.syntax.all.*
import me.katze.gui4s.example.MonadErrorT
import me.katze.gui4s.example.api.exported.{*, given}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{LinearLayout, Widget, drawDecorator}

type PaddingWidget[Widget, Padding] = Widget => Paddings[Padding] => Widget

def gapPaddingWidget[
  F[_] : Monad,
  Update[_] : Monad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
  PlaceError,
  T
](
  eventHandleDecorator :
    (widget : Widget.ValueWrapper[T, Update, SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent], shift : Point2d[MeasurementUnit]) =>
      Widget.ValueWrapper[T, Update, SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent],
  drawDecorations : (draw : Draw, shift : Point2d[MeasurementUnit]) => Draw
) : PaddingWidget[SkijaPlace[F, MeasurementUnit, PlaceError, Widget.ValueWrapper[T, Update, SkijaPlaceT[F, MeasurementUnit, PlaceError], Draw, RecompositionReaction, HandleableEvent]], MeasurementUnit] =
  initialWidget => paddings =>
    withBounds(
      initialWidget,
      _.cut(paddings.horizontalLength, paddings.verticalLength)
    ).map {
      case Sized(widget, size) =>

        Sized(
          drawDecorator(
            eventHandleDecorator(widget, paddings.topLeftCornerShift),
            drawDecorations(_, paddings.topLeftCornerShift)
          ),
          size + paddings.extraBoundsRect
        )
    }
end gapPaddingWidget

def paddingLayoutVerticalStrategy[
  Place[_] : MonadErrorT[Error],
  MeasurementUnit : Fractional as MUF,
  Error
](
  paddings: Paddings[Padding[MeasurementUnit]],
  error : Error
) : MainAxisPlacement[Place, MeasurementUnit] =
  (paddings.top, paddings.bottom) match
    case (Padding.Gap(_), _)            => MainAxisPlacement.Begin(MUF.zero)
    case (Padding.Fill, Padding.Gap(_)) => MainAxisPlacement.End(MUF.zero, error)
    case (Padding.Fill, Padding.Fill)   => MainAxisPlacement.Center(MUF.zero, error)
end paddingLayoutVerticalStrategy

def paddingLayoutHorizontalStrategy[
  Place[_] : MonadErrorT[Error],
  MeasurementUnit: Fractional,
  Error
](
    paddings: Paddings[Padding[MeasurementUnit]],
    error: Error
): AdditionalAxisPlacement[Place, MeasurementUnit] =
  (paddings.left, paddings.right) match
    case (Padding.Gap(_), _) => AdditionalAxisPlacement.Begin
    case (Padding.Fill, Padding.Gap(_)) => AdditionalAxisPlacement.End(error)
    case (Padding.Fill, Padding.Fill) => AdditionalAxisPlacement.Center(error)
end paddingLayoutHorizontalStrategy

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
    MeasurementUnit
  ],
  layout : LinearLayout[
    Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    OuterPlace,
    MeasurementUnit,
    Axis
  ],
  infinitePaddingInInfiniteContainer : PlaceError
) : PaddingWidget[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  Padding[MeasurementUnit]
] =
  widget => paddings =>
    layout(
      List(
        innerGaps(widget)(paddings.map(_.gapOrZero))
      ),
      Axis.Vertical,
      paddingLayoutVerticalStrategy(paddings, infinitePaddingInInfiniteContainer),
      paddingLayoutHorizontalStrategy(paddings, infinitePaddingInInfiniteContainer)
    )