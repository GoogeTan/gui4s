package me.katze.gui4s.example
package task

import cats.effect.Concurrent
import cats.syntax.all.{*, given}
import fs2.*

enum WidgetTaskImpl[+F[+_], +T]:
  case OneEvent(value : F[T])
  case ManyEvents(stream : Stream[F, T])
end WidgetTaskImpl

def runWidgetTask[F[+_] : Concurrent, T](widgetTask : WidgetTaskImpl[F, T], offerTask : T => F[Unit]) : F[Unit] =
  widgetTask match
    case WidgetTaskImpl.OneEvent(value) => value.flatMap(offerTask)
    case WidgetTaskImpl.ManyEvents(stream) => stream.evalMap(offerTask).compile.drain
  end match
end runWidgetTask