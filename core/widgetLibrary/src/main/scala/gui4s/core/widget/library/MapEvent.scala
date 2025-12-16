package gui4s.core.widget.library

type MapEvent[Widget[_]] = [A, B] => (A => B) => Widget[A] => Widget[B]
