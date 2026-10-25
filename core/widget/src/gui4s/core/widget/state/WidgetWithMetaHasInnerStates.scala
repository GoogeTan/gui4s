package gui4s.core.widget
package state

def widgetWithMetaHasInnerStates[Widget, Meta, RecompositionReaction](initial : HasInnerStates[Widget, RecompositionReaction]) : HasInnerStates[(Widget, Meta), RecompositionReaction] =
  self =>
    initial(self._1)
end widgetWithMetaHasInnerStates
  
