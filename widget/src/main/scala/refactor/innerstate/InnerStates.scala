package me.katze.gui4s.widget
package refactor.innerstate

trait InnerStates[Self, UpdatedSelf, RecompositionReaction]:
  def innerStates(self: Self) : Map[String, StateTree[RecompositionReaction]]
  def mergeWithOldInnerStates(self: Self, innerStates : Map[String, StateTree[RecompositionReaction]]) : UpdatedSelf
end InnerStates
