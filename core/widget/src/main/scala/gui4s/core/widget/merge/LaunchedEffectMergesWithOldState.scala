package gui4s.core.widget
package merge

import cats.Functor
import cats.syntax.all.*

import scala.reflect.Typeable

// TODO Думается мне, что это можно как-то разделить, но я пока не понимаю как.
@SuppressWarnings(Array("org.wartremover.warts.Any"))
def launchedEffectMergesWithOldState[Key, Place[_], Recomposition, Task, Widget](using Typeable[Key], Functor[Place])(
  keyTypeError : [T] => (Path, Any) => Place[T],
  widgetMergesWithOldState : MergesWithOldStates[Widget, Recomposition, Place[Widget]],
): MergesWithOldStates[
  (LaunchedEffect[Key, Task], Widget),
  Recomposition,
  Place[(LaunchedEffect[Key, Task], Widget)]
] =
  (tmp, pathToParent, oldStates) =>
    val (launchedEffectSelf, widgetSelf) = tmp
    oldStates.get(launchedEffectSelf.name).map(_.state) match
      case Some(key : Key) =>
        widgetMergesWithOldState(widgetSelf, pathToParent, oldStates).map(placedWidget => (launchedEffectSelf.copy(key = key), placedWidget))
      case Some(valueFound) =>
        keyTypeError(pathToParent.appendLast(launchedEffectSelf.name), valueFound)
      case None =>
        widgetMergesWithOldState(widgetSelf, pathToParent, oldStates).map(placedWidget => (launchedEffectSelf, placedWidget))
end launchedEffectMergesWithOldState