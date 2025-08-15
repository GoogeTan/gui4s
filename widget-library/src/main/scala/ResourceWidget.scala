package me.katze.gui4s.widget.library

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.Functor
import cats.data.*
import cats.syntax.all.*
import me.katze.gui4s.widget.Path

import scala.annotation.experimental
import scala.reflect.Typeable

@experimental
type ResourceWidget[Widget, F[_]] = [T : Typeable] => (name : String, resource : F[(T, F[Unit])]) => WithContext[Widget, Option[T]]


@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
given destructableIsTypeable[T: Typeable, IO: Typeable]: Typeable[(T, IO)] = x => x match {
  case (a: T, io: IO) =>
    Some[(T, IO)]((a, io)).map(_.asInstanceOf[x.type & (T, IO)])
  case _ => None
}

@experimental
def resourceWidget[
  Widget[_],
  Update[_, _] : BiMonad,
  IO[_] : Functor,
  Event,
](
    transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Update],
    launchedEffect : [TaskEvent : Typeable] => (name : String, child : Widget[Event], task : IO[TaskEvent]) => Widget[Either[TaskEvent, Event]],
    doubleAllocError : [T] => Path => Update[Event, T]
)(using Typeable[IO[Unit]]) : ResourceWidget[Widget[Event], IO] =
  [Value : Typeable] => (name, resource) =>
      (widget : Option[Value] => Widget[Event]) =>
          transitiveStatefulWidget[
            Option[(Value, IO[Unit])],
            Event,
            (Value, IO[Unit])
          ](
            name = name,
            initialState = None,
            eventHandler = {
              case (None, _, NonEmptyList(event, Nil)) =>
                Some(event).pure[Update[Event, *]]
              case (_, path, _) => doubleAllocError(path.appendLast(name))
            },
            body = state =>
              launchedEffect[(Value, IO[Unit])](
                "effect_launcher",
                widget(state.map(_._1)),
                resource
              )
          )
end resourceWidget