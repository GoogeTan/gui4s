package me.katze.gui4s.widget
package recomposition

import scala.reflect.Typeable

def launchedEffectReactsOnRecomposition[
  Place,
  Recomposition,
  Key : {Typeable, Equiv as eq}
](
  emptyRecomposition : Recomposition,
  keysTypeMismatchError : Recomposition
) : ReactsOnRecomposition[LaunchedEffect[Key, Recomposition], Recomposition] =
  (self, pathToParent, states) =>
    states.get(self.name) match
      case Some(key : Key) =>
        if eq.equiv(key, self.key) then
          emptyRecomposition
        else
          self.taskOnChange(pathToParent.appendLast(self.name))
      case Some(_) =>
        keysTypeMismatchError
      case None =>
        self.taskOnChange(pathToParent.appendLast(self.name))
    end match
end launchedEffectReactsOnRecomposition

