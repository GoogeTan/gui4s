package me.katze.gui4s.widget
package impl

import fs2.*

enum WidgetTaskImpl[+F[+_], +T]:
  case OneEvent(value : F[T])
  case ManyEvents(stream : Stream[F, T])
end WidgetTaskImpl
