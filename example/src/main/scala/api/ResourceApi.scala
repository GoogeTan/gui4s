package me.katze.gui4s.example
package api

import cats.Applicative
import cats.effect.Resource
import cats.syntax.all.*
import me.katze.gui4s.widget.stateful.{EventReaction, RichTypeChecker, given}
import me.katze.gui4s.widget.given

trait ResourceApi[Widget[_], WidgetTask[_], F[_] : Applicative, Recomposition](
  effectApi: EffectApi[Widget, WidgetTask],
  statefulApi: StatefulApi[Widget, WidgetTask, Recomposition],
  mapWidgetLibrary: MapWidgetLibrary[Widget],
  runFInRecompisition : F[Unit] => Recomposition,

)(using FRTC : RichTypeChecker[F[Unit]]):
  def resource[
    Event : RichTypeChecker as EventRTC,
    T : {Equiv as eq, RichTypeChecker as TRTC}
  ](
    name : String,
    resource : Resource[F, T],
    body : Option[T] => Widget[Event]
) : Widget[Event] =
    given Equiv[(T, F[Unit])] = (a, b) => eq.equiv(a._1, b._1)
    statefulApi.stateful[Option[(T, F[Unit])], Event, Either[Event, (T, F[Unit])]](
      name,
      None,
      {
        case Some(value) => runFInRecompisition(value._2)
        case None => runFInRecompisition(().pure[F])
      },
      (state, event) =>
        event match
          case Left(value) => EventReaction(state, List(value), Nil)
          case Right(value) => EventReaction(Some(value), Nil, Nil)
    ):
      case Some((innerState, _)) =>
        mapWidgetLibrary.mapEvent(
          body(Some(innerState))
        )(Left(_))
      case None =>
        mapWidgetLibrary.mapEvent(
          body(None)
        )(Left(_))
  end resource
end ResourceApi
