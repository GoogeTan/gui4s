package me.katze.gui4s.widget
package library

import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.*
import cats.data.*

import draw.{*, given}
import handle.{*, given}
import recomposition.{*, given}
import free.{*, given}
import state.{*, given}
import merge.{*, given}

def basicDecoratorWithRect[
  Update[_] : Monad as M,
  Place[_] : Functor as PF,
  InnerPlace[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
    mark : String,
    original : Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]],
    f : InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]] => InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]
) : Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]] =
  basicPlaceDecorator[
    Update,
    Place * InnerPlace,
    Draw,
    RecompositionReaction,
    HandleableEvent,
  ](mark, original, PF.map(_)(f))
end basicDecoratorWithRect

def basicDecorator[
  Update[_] : Functor as M,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
    mark : String,
    original : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    f : Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent] => Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]
) : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  basicPlaceDecorator(mark, original, _.map(f))
end basicDecorator

def basicPlaceDecorator[
  Update[_] : Functor as M,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
    mark : String,
    original : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    f : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] => Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]
) : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
    f(
      original.map(
        placedOriginal =>
          Widget.ValueWrapper(
            valueToDecorate = (mark, placedOriginal),
            valueAsFree = (a, widget) =>
              basicPlaceDecorator(mark, widgetAsFree[Update, Place, Draw, RecompositionReaction, HandleableEvent](widget), f).map(pl => (a, pl)),
            valueIsDrawable = drawableIsContravariant.contramap(widgetIsDrawable[Update, Place, Draw, RecompositionReaction, HandleableEvent])(_._2),
            valueHandlesEvent =
              (self, path, event) =>
                M.map(
                  widgetHandlesEvent(using M, PF)(
                    self._2, path, event
                  )
                )(v => f(v).map(vv => (self._1, vv))),
            valueMergesWithOldState = mergesWithOldStatesIsStrong.second(widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent]),
            valueReactsOnRecomposition = reactsOnRecompositionIsContravariant.contramap(widgetReactsOnRecomposition[Update, Place, Draw, RecompositionReaction, HandleableEvent])(_._2),
            valueHasInnerState = hasInnerStatesIsContravariantMonoidal.contramap(widgetHasInnerStates[Update, Place, Draw, RecompositionReaction, HandleableEvent])(_._2)
          )
      )
    )
end basicPlaceDecorator
