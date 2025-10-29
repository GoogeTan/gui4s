package gui4s.core.widget
package merge

import gui4s.core.widget.free.AsFree

def anyHasNothingToMerge[A, B](asFree : AsFree[A, B]) : MergesWithOldStates[A, Any, B] =
  (self, _, _) => asFree(self)
end anyHasNothingToMerge
