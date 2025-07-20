package me.katze.gui4s.widget
package merge

import cats.Functor
import cats.syntax.all.*

import scala.reflect.Typeable

// TODO Думается мне, что это можно как-то разделить, но я пока не понимаю как.
def launchedEffectMergesWithOldState[Key, Place[_], Recomposition, Task, Widget](using Typeable[Key], Functor[Place])(
  keyTypeError : [T] => Path => Place[T],
  widgetMergesWithOldState : MergesWithOldStates[Widget, Recomposition, Place[Widget]],
): MergesWithOldStates[
  (LaunchedEffect[Key, Task], Widget),
  Recomposition,
  Place[(LaunchedEffect[Key, Task], Widget)]
] =
  (tmp, pathToParent, oldStates) =>
    val (launchedEffectSelf, widgetSelf) = tmp
    oldStates.get(launchedEffectSelf.name) match
      case Some(key : Key) =>
        widgetMergesWithOldState(widgetSelf, pathToParent, oldStates).map(placedWidget => (launchedEffectSelf.copy(key = key), placedWidget))
      case Some(_) =>
        keyTypeError(pathToParent.appendLast(launchedEffectSelf.name))
      case None =>
        widgetMergesWithOldState(widgetSelf, pathToParent, oldStates).map(placedWidget => (launchedEffectSelf, placedWidget))
end launchedEffectMergesWithOldState
