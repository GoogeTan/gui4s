package gui4s.core.widget.library

import catnip.BiMonad
import cats.Functor
import cats.data.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.{TransitiveStatefulWidget, WithContext}

import scala.reflect.Typeable

type ResourceWidget[Widget, F[_]] = [T : Typeable] => (name : String, resource : F[(T, F[Unit])]) => WithContext[Widget, Option[T]]

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
given destructableIsTypeable[T: Typeable, IO: Typeable]: Typeable[(T, IO)] = x => x match {
  case (a: T, io: IO) =>
    Some[(T, IO)]((a, io)).map(_.asInstanceOf[x.type & (T, IO)])
  case _ => None
}

def resourceWidget[
  Widget[_],
  Update[_, _] : BiMonad as updateBiMonad,
  Place[_],
  IO[_] : Functor,
  Event,
](
    transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Update, Nothing, Nothing],
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
                updateBiMonad[Event]().pure(Some(event))
              case (_, path, _) => doubleAllocError(path)
            },
            body = state =>
              launchedEffect[(Value, IO[Unit])](
                "effect_launcher",
                widget(state.map(_._1)),
                resource
              )
          )
end resourceWidget