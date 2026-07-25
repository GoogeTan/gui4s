package gui4s.core.widget.library

import catnip.CodensityT

import gui4s.core.widget.StatefulState

type MergeStates[Place[_], State] = [T] => (StatefulState[State], StatefulState[State], StatefulState[State] => Place[T]) => Option[Place[T]]
type MergeStates2[Place[_], State] =
  (StatefulState[State], StatefulState[State]) => CodensityT[Place, StatefulState[State]]