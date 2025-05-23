package me.katze.gui4s.widget
package state

type HasInnerStates[T, RecompositionReaction] =
  T => Map[String, StateTree[RecompositionReaction]]