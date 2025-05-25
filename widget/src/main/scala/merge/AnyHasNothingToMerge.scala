package me.katze.gui4s.widget
package merge

import free.AsFree

def anyHasNothingToMerge[A, B](asFree : AsFree[A, B]) : MergesWithOldStates[A, Any, B] =
  (self, _, _) => asFree(self)
end anyHasNothingToMerge
