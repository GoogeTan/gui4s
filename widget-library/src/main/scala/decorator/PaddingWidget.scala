package me.katze.gui4s.widget.library.decorator

import catnip.syntax.additional.*
import catnip.syntax.all.given
import catnip.syntax.monad.MonadErrorT
import cats.syntax.all.*
import cats.{Comonad, Functor, Id}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.layout.given
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement}
import me.katze.gui4s.widget.library.decorator.Decorator
import me.katze.gui4s.widget.library.decorator.Decorator.given
import me.katze.gui4s.widget.library.{LinearLayout, Widget, WidgetHandlesEvent}

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

def paddingLayoutVerticalStrategy[
  Place[_] : MonadErrorT[Error],
  MeasurementUnit : Fractional as MUF,
  Error
](
  paddings: Paddings[Padding[MeasurementUnit]],
  error : Error
) : MainAxisPlacement[Place, Id, MeasurementUnit] =
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
    Paddings[MeasurementUnit]
  ],
  layout : LinearLayout[
    Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    OuterPlace,
    Id,
    MeasurementUnit,
    Axis
  ],
  infinitePaddingInInfiniteContainer : PlaceError
) : PaddingWidget[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  Paddings[Padding[MeasurementUnit]]
] =
  paddings => widget =>
    layout(       
      innerGaps(paddings.map(_.gapOrZero))(widget),
      Axis.Vertical,
      paddingLayoutVerticalStrategy(paddings, infinitePaddingInInfiniteContainer),
      paddingLayoutHorizontalStrategy(paddings, infinitePaddingInInfiniteContainer)
    )
end paddingWidget