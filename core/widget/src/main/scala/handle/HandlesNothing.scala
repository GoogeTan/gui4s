package gui4s.core.widget
package handle

import free.AsFree

def handlesNothing[A, Event, B](asFree : AsFree[A, B]) : HandlesEvent[A, Event, B] =
  (self, _, _) => asFree(self)
end handlesNothing
