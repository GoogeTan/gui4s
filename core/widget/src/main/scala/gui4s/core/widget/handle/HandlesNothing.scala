package gui4s.core.widget
package handle

import cats.Applicative
import gui4s.core.widget.free.AsFree

def handlesNothing[A, Event, B](asFree : AsFree[A, B]) : HandlesEvent[A, Event, B] =
  (self, _, _) => asFree(self)
end handlesNothing

def handlesNothing[A, Event, B, Update[_] : Applicative as A] : HandlesEvent[A, Event, Update[Option[B]]] =
  (_, _, _) => A.pure(None)
end handlesNothing
