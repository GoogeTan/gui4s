package gui4s.core.widget
package handle

def pureEventHandler[Self, Event, NewSelf](f : Self => NewSelf) : HandlesEvent[Self, Event, NewSelf] =
  (self, _, _) => f(self)
end pureEventHandler

