package gui4s.decktop.widget.library

import catnip.syntax.additional.*
import catnip.syntax.all.given
import cats.*
import cats.arrow.Strong
import cats.syntax.all.*
import gui4s.core.widget.handle.given
import gui4s.core.widget.merge.launchedEffectMergesWithOldState
import gui4s.core.widget.recomposition.{*, given}
import gui4s.core.widget.state.{*, given}
import gui4s.core.widget.{LaunchedEffect, Path}

import scala.reflect.Typeable

type LaunchedEffectWidget[Widget, Key, Task] = (name : String, child : Widget, key : Key, task : Task) => Widget

// TODO надо написать в доке, что он не гарантиурет остановку ИО при смене ключа
def launchedEffect[
  Update[_] : Applicative as UA,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  Key : {Typeable as KT, Equiv}
](
  keyTypeError : [T] => (Path, Any) => Place[T],
  keysTypeMismatchError : Any => RecompositionReaction,
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
          valueHandlesEvent = handlesEventFIsStrong[Update * Place, HandlableEvent](using nestedFunctorsAreFunctors[Update, Place]).second(widgetHandlesEvent(using UA, PF)),
          valueMergesWithOldState = launchedEffectMergesWithOldState(using KT, PF)(keyTypeError, widgetMergesWithOldState),
          valueReactsOnRecomposition = reactsOnRecompositionIsContravariantMonoidal.product(
            launchedEffectReactsOnRecomposition(M.empty, keysTypeMismatchError),
            widgetReactsOnRecomposition
          ),
          valueHasInnerState = hasInnerStatesIsContravariantMonoidal.product(launchedEffectHasInnerState(M.empty), widgetHasInnerStates),
        )