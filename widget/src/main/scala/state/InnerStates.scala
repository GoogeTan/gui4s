package me.katze.gui4s.widget
package state

type InnerStates[T, RecompositionReaction] =
  T => Map[String, StateTree[RecompositionReaction]]