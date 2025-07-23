package me.katze.gui4s.widget.library

import cats.*
import cats.arrow.{Profunctor, Strong}
import cats.syntax.all.*
import catnip.syntax.all.{*, given}
import me.katze.gui4s.widget.{LaunchedEffect, Path}
import me.katze.gui4s.widget.handle.{*, given}
import me.katze.gui4s.widget.merge.launchedEffectMergesWithOldState
import me.katze.gui4s.widget.recomposition.{*, given}
import me.katze.gui4s.widget.state.{*, given}

import scala.reflect.Typeable

type LaunchedEffectWidget[Widget, Key, Task] = (name : String, child : Widget, key : Key, task : Task) => Widget

def launchedEffect[
  Update[_] : Applicative as UA,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  Key : {Typeable as KT, Equiv}
](
  keyTypeError : [T] => Path => Place[T],
  keysTypeMismatchError : RecompositionReaction,
) : LaunchedEffectWidget[Place[Widget[Update, Place, Draw, RecompositionReaction, HandlableEvent]], Key, Path => RecompositionReaction] =
  type PlacedWidget = Widget[Update, Place, Draw, RecompositionReaction, HandlableEvent]
  (name, freeChild, key, task) =>
    freeChild.map:
      child =>
        Widget.ValueWrapper[
          (
            LaunchedEffect[
              Key, 
              Path => RecompositionReaction,
            ],
            PlacedWidget
          ),
          Update, 
          Place, 
          Draw, 
          RecompositionReaction,
          HandlableEvent
        ](
          valueToDecorate = (LaunchedEffect(name, key, task), child),
          valueAsFree = Strong[[A, B] =>> A => Place[B]].second(widgetAsFree(using PF)),
          valueIsDrawable = Contravariant[[A] =>> A => Draw].contramap(widgetIsDrawable)(_._2),
          valueHandlesEvent = handlesEventFIsStrong.second(widgetHandlesEvent(using UA, PF)),
          valueMergesWithOldState = launchedEffectMergesWithOldState(using KT, PF)(keyTypeError, widgetMergesWithOldState),
          valueReactsOnRecomposition = reactsOnRecompositionIsContravariantMonoidal.product(
            launchedEffectReactsOnRecomposition(M.empty, keysTypeMismatchError),
            widgetReactsOnRecomposition
          ),
          valueHasInnerState = hasInnerStatesIsContravariantMonoidal.product(launchedEffectHasInnerState(M.empty), widgetHasInnerStates),
        )