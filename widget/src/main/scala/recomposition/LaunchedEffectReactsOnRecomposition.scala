package me.katze.gui4s.widget
package recomposition

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
def launchedEffectReactsOnRecomposition[
  Recomposition,
  Key : {Typeable, Equiv as eq},
](
  emptyRecomposition : Recomposition,
  keysTypeMismatchError : Any => Recomposition,
) : ReactsOnRecomposition[LaunchedEffect[Key, Path => Recomposition], Recomposition] =
  (self, pathToParent, states) =>
    states.get(self.name).map(_.state) match
      case Some(key : Key) =>
        if eq.equiv(key, self.key) then
          emptyRecomposition
        else
          self.taskOnChange(pathToParent.appendLast(self.name))
      case Some(valueFound : Any) =>
        keysTypeMismatchError(valueFound)
      case None =>
        println("task started")
        self.taskOnChange(pathToParent.appendLast(self.name))
    end match
end launchedEffectReactsOnRecomposition

