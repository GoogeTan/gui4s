package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import catnip.syntax.all.given
import catnip.syntax.monad.MonadErrorT
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{LinearLayout, Widget}

type PaddingWidget[Widget, Padding] = Widget => Paddings[Padding] => Widget

def gapPaddingWidget[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  MeasurementUnit : Numeric,
](
  eventHandleDecorator :
    (widget : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]], shift : Point2d[MeasurementUnit]) =>
      Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  drawDecorations : (draw : Draw, shift : Point2d[MeasurementUnit]) => Draw,
  placementShift : [T] => (Place[T], Paddings[MeasurementUnit]) => Place[T]
) : PaddingWidget[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]], MeasurementUnit] =
  initialWidget => paddings =>
    eventHandleDecorator(
      placementShift(
        initialWidget,
        paddings
      ).map(
        placedWidget =>
          final case class GapWidget(currentWidget: Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent])
          Widget.ValueWrapper[
            GapWidget,
            Update,
            Place,
            Draw,
            RecompositionReaction,
            HandleableEvent
          ](
            valueToDecorate = GapWidget(placedWidget),
            valueAsFree = placed => placed.currentWidget.asFree.map(GapWidget(_)),
            valueIsDrawable = self => drawDecorations(self.currentWidget.draw, paddings.topLeftCornerShift),
            valueHandlesEvent = (self, path, event) => self.currentWidget.handleEvent(path, event).map(_.map(GapWidget(_))),
            valueMergesWithOldState = (self, path, states) =>
              self.currentWidget.mergeWithOldState(path, states).map(GapWidget(_)),
            valueReactsOnRecomposition = (self, path, states) =>
              self.currentWidget.reactOnRecomposition(path, states),
            valueHasInnerState =
              self => self.currentWidget.innerStates
          )
        ),
        paddings.topLeftCornerShift
      )
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
end paddingWidget