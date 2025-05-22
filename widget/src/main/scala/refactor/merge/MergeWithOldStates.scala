package me.katze.gui4s.widget
package refactor.merge

type MergesWithOldStates[Self, RecompositionAction, UpdatedSelf] =
  (self : Self, oldInnerStates : Map[String, StateTree[RecompositionAction]]) => UpdatedSelf
