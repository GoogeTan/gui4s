package me.katze.gui4s.widget
package merge

type MergesWithOldStates[-Self, -RecompositionAction, +UpdatedSelf] =
  (self : Self, pathToParent : Path, oldInnerStates : Map[String, StateTree[RecompositionAction]]) => UpdatedSelf
