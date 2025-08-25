package gui4s.core.widget
package state

def hasNoInnerState[T] : HasInnerStates[T, Nothing] =
  _ => Map()
end hasNoInnerState