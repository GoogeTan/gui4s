package me.katze.gui4s.widget
package state

def hasNoInnerState[T] : HasInnerStates[T, Nothing] =
  _ => Map()
end hasNoInnerState