package gui4s.core.widget
package handle

def mapEventHandle[T, EnvironmentalEvent, UpdatedWidget, NewUpdatedWidget](
   original : HandlesEvent[T, EnvironmentalEvent, UpdatedWidget]
)(
  f : UpdatedWidget => NewUpdatedWidget
) : HandlesEvent[T, EnvironmentalEvent, NewUpdatedWidget] =
  (self, path, event) => f(original(self, path, event))
end mapEventHandle


def mapEventHandle_[
  T,
  UpdatedWidget, 
  NewUpdatedWidget
](
  original : HandlesEvent_[T, UpdatedWidget]
)(
  f : UpdatedWidget => NewUpdatedWidget
) : HandlesEvent_[T, NewUpdatedWidget] =
  (self, path) => f(original(self, path))
end mapEventHandle_