package me.katze.gui4s.widget
package handle

def mapEventHandle[T, HandleableEvent, UpdatedWidget, NewUpdatedWidget](
 original : HandlesEvent[T, HandleableEvent, UpdatedWidget]
)(
  f : UpdatedWidget => NewUpdatedWidget
) : HandlesEvent[T, HandleableEvent, NewUpdatedWidget] =
  (self, path, event) => f(original(self, path, event))
end mapEventHandle