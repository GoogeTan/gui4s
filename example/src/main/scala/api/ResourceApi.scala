package me.katze.gui4s.example
package api

import cats.effect.Resource
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.stateful.{EventReaction, RichTypeChecker, given}

/*
 * Этот код, очевидно, не протестируешь. Надо отрефакторить
 */
def resource[
  Widget[_] : MapWidgetLibrary,
  WidgetTask[_],
  F[_],
  Recomposition : Empty,
  Event: RichTypeChecker as EventRTC,
  State: {Equiv as eq, RichTypeChecker as TRTC},
  Supervisor
](
  statefulApi: StatefulApi[Widget, WidgetTask, Recomposition, Supervisor],
  runFInRecompisition : F[Unit] => Recomposition,
)(
  name: String,
  resource: Resource[F, State],
  body: Option[State] => Widget[Event],
  globalSupervisor : Supervisor
)(
  using RichTypeChecker[F[Unit]]
): Widget[Event] =
  statefulApi.stateInBetween[Option[(State, F[Unit])], Event, (State, F[Unit])](
    name,
    None,
    {
      case Some((_, dealloc)) => runFInRecompisition(dealloc)
      case None => empty
    },
    (_, newState) =>
      EventReaction(Some(newState), Nil, Nil),
    globalSupervisor
  ):
    case Some((innerState, _)) =>
      body(Some(innerState)).mapEvent(Left(_))
    case None =>
      body(None).mapEvent(Left(_))
end resource
