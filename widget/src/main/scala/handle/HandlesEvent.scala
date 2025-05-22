package me.katze.gui4s.widget
package handle

type HandlesEvent[T, HandleableEvent, UpdatedWidget] = (self : T, pathToParent : Path, event : HandleableEvent) => UpdatedWidget