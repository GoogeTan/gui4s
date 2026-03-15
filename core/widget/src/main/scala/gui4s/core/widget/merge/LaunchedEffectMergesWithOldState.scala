package gui4s.core.widget
package merge

import scala.reflect.Typeable

import cats.Functor
import cats.syntax.all._

import gui4s.core.widget.free.AsFreeF

// TODO Думается мне, что это можно как-то разделить, но я пока не понимаю как.
@SuppressWarnings(Array("org.wartremover.warts.Any"))
def launchedEffectMergesWithOldState[Key : Equiv as KE, Place[_], Recomposition, Task, Widget](using Typeable[Key], Functor[Place])(
  keyTypeError : [T] => (Path, Any) => Place[T],
  widgetMergesWithOldState : MergesWithOldStates[Widget, Recomposition, Option[Place[Widget]]],
  widgetAsFree : AsFreeF[Widget, Place]
): MergesWithOldStates[
  (LaunchedEffect[Key, Task], Widget),
  Recomposition,
  Option[Place[(LaunchedEffect[Key, Task], Widget)]]
] =
  (tmp, pathToParent, oldStates) =>
    val (launchedEffectSelf, widgetSelf) = tmp
    oldStates.get(launchedEffectSelf.name).map(_.state) match
      case Some(key : Key) =>
        widgetMergesWithOldState(widgetSelf, pathToParent, oldStates) match
          case Some(value) =>
            Some(value.map(placedWidget => (launchedEffectSelf.copy(key = key), placedWidget)))
          case None =>
            if KE.equiv(launchedEffectSelf.key, key) then 
              None
            else 
              Some(
                widgetAsFree(widgetSelf).map((launchedEffectSelf.copy(key = key), _))
              )
      case Some(valueFound) =>
        Some(keyTypeError(pathToParent / launchedEffectSelf.name, valueFound))
      case None =>
        widgetMergesWithOldState(widgetSelf, pathToParent, oldStates)
          .map(
            _.map(placedWidget => (launchedEffectSelf, placedWidget))
          )
end launchedEffectMergesWithOldState