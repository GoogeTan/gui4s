package me.katze.gui4s.widget
package refactor.state

type InnerStates[T, RecompositionReaction] = 
  T => Map[String, StateTree[RecompositionReaction]]