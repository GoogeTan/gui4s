package me.katze.gui4s.widget
package handle

type HandlesEvent[-T, -HandleableEvent, +UpdatedWidget] = (self : T, pathToParent : Path, event : HandleableEvent) => UpdatedWidget
type HandlesEventF[T, -HandleableEvent, +Update[_]] = HandlesEvent[T, HandleableEvent, Update[T]]