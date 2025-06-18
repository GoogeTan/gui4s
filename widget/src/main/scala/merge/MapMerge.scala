package me.katze.gui4s.widget
package merge

def mapMerge[Self, RecompositionAction, UpdatedSelf, NewSelf](old : MergesWithOldStates[Self, RecompositionAction, UpdatedSelf])(f : UpdatedSelf => NewSelf) : MergesWithOldStates[Self, RecompositionAction, NewSelf] =
  (self, pathToParent, oldInnerStates) => 
    f(old(self, pathToParent, oldInnerStates))
end mapMerge
