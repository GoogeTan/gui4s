package me.katze.gui4s.widget
package handle

type HandlesEvent[-T, -HandleableEvent, +UpdatedWidget] = (self : T, pathToParent : Path, event : HandleableEvent) => UpdatedWidget
type HandlesEventF[T, -HandleableEvent, +Update[_]] = HandlesEvent[T, HandleableEvent, Update[T]]

extension[T, Event, NewT](value : HandlesEvent[T, Event, NewT])
  // TODO Я не знаю, почему встроенный метод не работает, так что так
  infix def andThen[NewNewT](f : NewT => NewNewT) : HandlesEvent[T, Event, NewNewT] =
    (self, path, event) => f(value(self, path, event))
  end andThen
end extension