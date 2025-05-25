package me.katze.gui4s.widget
package merge

import free.AsFree

import scala.reflect.Typeable

def launchedEffectMergesWithOldState[Key : Typeable, Place[+_], Recomposition](
  launchedEffectAsFree: AsFree[LaunchedEffect[Key, Recomposition], Place[LaunchedEffect[Key, Recomposition]]],
  keyTypeError : Path => Place[Nothing]                                                                           
): MergesWithOldStates[
  LaunchedEffect[Key, Recomposition],
  Recomposition,
  Place[LaunchedEffect[Key, Recomposition]]
] =
  (self, pathToParent, oldStates) =>
    oldStates.get(self.name) match
      case Some(key : Key) => 
        launchedEffectAsFree(self.copy(key = key))
      case Some(_) => 
        keyTypeError(pathToParent.appendLast(self.name))
      case None =>
        launchedEffectAsFree(self)
end launchedEffectMergesWithOldState
