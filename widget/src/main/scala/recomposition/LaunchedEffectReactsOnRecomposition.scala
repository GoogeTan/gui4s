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
          self.taskOnChange(pathToParent)
      case Some(valueFound : Any) =>
        keysTypeMismatchError(valueFound)
      case None =>
        self.taskOnChange(pathToParent)
    end match
end launchedEffectReactsOnRecomposition

