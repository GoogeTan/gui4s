package me.katze.gui4s.example
package api.exported

import catnip.{BiMonad, FFI}
import catnip.syntax.all.{*, given}
import cats.arrow.FunctionK
import cats.{Applicative, Apply, FlatMap, Functor, InjectK, Monad, ~>}
import cats.data.{EitherT, ReaderT, StateT}
import cats.effect.ExitCode
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.{CatchEvents, EventReaction, Path}
import cats.syntax.all.*
import io.github.humbleui.skija.PathMeasure
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.example.place.RunPlacement
import me.katze.gui4s.example.update.ApplicationRequest
import me.katze.gui4s.layout.{Point3d, Sized, given}
import me.katze.gui4s.widget.library.Widget_

opaque type SkijaUpdate[MeasurementUnit, Event, Value] = EventResult[MeasurementUnit, Event, Value]
type SkijaUpdateT[MeasurementUnit, Event] = SkijaUpdate[MeasurementUnit, Event, *]

opaque type SkijaPlaceInner[IO[_], MeasurementUnit, Error, Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaPlaceInnerT[IO[_], MeasurementUnit, Error] = SkijaPlaceInner[IO, MeasurementUnit, Error, *]
type SkijaPlace[IO[_], MeasurementUnit, Error, Value] = SkijaPlaceInner[IO, MeasurementUnit, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], MeasurementUnit, Error] = SkijaPlace[IO, MeasurementUnit, Error, *]

type SkijaRecomposition[F[_]] = F[Unit]

type SkijaPlacedWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = Widget_[SkijaUpdateT[MeasurementUnit, Event], SkijaPlaceT[F, MeasurementUnit, PlaceError], SkijaDraw[F, OglWindow], SkijaRecomposition[F], DownEvent]
type SkijaWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = SkijaPlace[F, MeasurementUnit, PlaceError, SkijaPlacedWidget[F, MeasurementUnit, PlaceError, Event, DownEvent]]

type GetBounds[F[_], MeasurementUnit] = F[Bounds[MeasurementUnit]]
type SetBounds[F[_], MeasurementUnit] = Bounds[MeasurementUnit] => F[Unit]
type SizeText[F[_], G[_]] = (ffi: FFI[F], text: String, shaper: Shaper, options: SkijaTextStyle) => G[SkijaPlacedText]

given[MeasurementUnit] : BiMonad[SkijaUpdate[MeasurementUnit, *, *]] = summon

def markEventHandled[MeasurementUnit, Event] : SkijaUpdate[MeasurementUnit, Event, Unit] =
  StateT.modify(me.katze.gui4s.example.markEventHandled)
end markEventHandled

def getCoordinates[MeasurementUnit, Event] : SkijaUpdate[MeasurementUnit, Event, Point3d[MeasurementUnit]] =
  StateT.get[EventResult_[Event, *], EventResultState[MeasurementUnit]].map(_.widgetCoordinates)
end getCoordinates

def raiseEvents[MeasurementUnit, Event](events : List[Event]) : SkijaUpdate[MeasurementUnit, Event, Unit] =
  StateT.modify(
    state => (state.consumed, state.widgetCoordinates)
  )
end raiseEvents

def mapEvents[MeasurementUnit, Event1, Event2, T](f : Event1 => Event2)(skijaUpdate : SkijaUpdate[MeasurementUnit, Event1, T]) : SkijaUpdate[MeasurementUnit, Event2, T] =
  skijaUpdate.catchEvents.flatMap((newEvents, value) => raiseEvents[MeasurementUnit, Event2](newEvents.map(f)).as(value))
end mapEvents

def raiseError[IO[_] : Monad, MeasurementUnit, PlaceError](error : => PlaceError) : SkijaPlaceInner[IO, MeasurementUnit, PlaceError, Nothing] =
  StateT.liftF(EitherT.left(error.pure))
end raiseError

def runPlaceStateT[IO[_] : FlatMap, MeasurementUnit](
                                                      bounds : IO[Bounds[MeasurementUnit]]
                                                    ) : RunPlacement[StateT[IO, Bounds[MeasurementUnit], *], IO] =
  [Value] => (toPlace : StateT[IO, Bounds[MeasurementUnit], Value]) =>
    bounds
      .flatMap(bounds => toPlace.run(bounds))
      .map(_._2)
end runPlaceStateT

def skijaInnerRunPlace[IO[_] : Monad, MeasurementUnit, PlaceError](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceInnerT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]] =
  runPlaceStateT[EitherT[IO, PlaceError, *], MeasurementUnit](EitherT.liftF(bounds))
end skijaInnerRunPlace

def skijaRunPlace[IO[_] : Monad, MeasurementUnit, PlaceError](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]] =
  [Value] => (value : SkijaPlaceInner[IO, MeasurementUnit, PlaceError, Sized[MeasurementUnit, Value]]) =>
    skijaInnerRunPlace(bounds)(value.map(_.value))
end skijaRunPlace

def runPlaceLift[U[_], F[_], G[_]](original : RunPlacement[F, U], inj : G ~> F) : RunPlacement[G, U] =
  [Value] => (value : G[Value]) => original(inj(value))
end runPlaceLift


def getBoundsStateT[F[_] : Applicative, MeasurementUnit] : GetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
  StateT.get
end getBoundsStateT

def setBoundsStateT[F[_] : Applicative, MeasurementUnit] : SetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
  StateT.set
end setBoundsStateT

def skijaGetBounds[IO[_] : Monad, MeasurementUnit, PlaceError] : GetBounds[SkijaPlaceInner[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
  getBoundsStateT[EitherT[IO, PlaceError, *], MeasurementUnit]
end skijaGetBounds

def skijaSetBounds[IO[_] : Monad, MeasurementUnit, PlaceError] : SetBounds[SkijaPlaceInner[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
  setBoundsStateT[EitherT[IO, PlaceError, *], MeasurementUnit]
end skijaSetBounds

def getBounds[F[_], G[_], MeasurementUnit](original : GetBounds[F, MeasurementUnit], inj : F ~> G) : GetBounds[G, MeasurementUnit] =
  inj(original)
end getBounds

def setBounds[F[_], G[_], MeasurementUnit](original : SetBounds[F, MeasurementUnit], inj : F ~> G) : SetBounds[G, MeasurementUnit] =
  bounds => inj(original(bounds))
end setBounds

given[F[_] : Monad, MeasurementUnit, Error] : Monad[SkijaPlaceInner[F, MeasurementUnit, Error, *]] = summon


def sizeTextStateT[F[_] : Applicative] : SizeText[F, [Value] =>> StateT[F, Bounds[Float], Sized[Float, Value]]] =
  (ffi : FFI[F], text: String, shaper : Shaper, options: SkijaTextStyle) =>
    StateT(
      bounds =>
        placeText(ffi = ffi,
          shaper = shaper,
          text = text,
          style = options,
          maxWidth = bounds.horizontal.max
        ).map((placedText) => (bounds, new Sized(placedText.text, placedText.width, placedText.height)))
    )
end sizeTextStateT

def sizeTextLift[U[_], F[_], G[_]](original : SizeText[U, F], inj : F ~> G) : SizeText[U, G] =
  (ffi, text, shaper, options) =>
    inj(original(ffi, text, shaper, options))
end sizeTextLift

def skijaSizeText[IO[_] : Monad, PlaceError] : SizeText[IO, SkijaPlace[IO, Float, PlaceError, *]] =
  sizeTextLift(sizeTextStateT[IO], mapF(EitherT.liftK[IO, PlaceError]))
end skijaSizeText

def mapF[F[_] : FlatMap, G[_] : Applicative, U[_], S](f : F ~> G) : (StateT[F, S, *] * U) ~> (StateT[G, S, *] * U) =
  new ~>[StateT[F, S, *] * U,  StateT[G, S, *] * U]:
    override def apply[A](fa: StateT[F, S, U[A]]): StateT[G, S, U[A]] =
      StateT(state =>
        f(fa.run(state))
      )
    end apply
  end new
end mapF

given[MeasurementUnit] : CatchEvents[SkijaUpdate[MeasurementUnit, *, *]] = summon


def runEventReaction[MeasurementUnit, T, Event](reaction : EventReaction[T, Event, ?], path : Path) : SkijaUpdate[MeasurementUnit, Event, T] =
  StateT(isEventHandled => EventResult_((isEventHandled, reaction.newState), reaction.parentEvent))
end runEventReaction

def handleApplicationRequests[F[_] : Monad, MeasurementUnit : Numeric as N] : [T] => SkijaUpdate[MeasurementUnit, ApplicationRequest, T] => F[Either[ExitCode, T]] =
  [T] => update =>
    val reaction : EventResult_[ApplicationRequest, (EventResultState[MeasurementUnit], T)] = update.run(emptyEventResultState)
    reaction.events.foldM(Right(reaction.widget._2))((_, request) =>
      request match
        case ApplicationRequest.CloseApp(code) => Left(code).pure[F]
    )
end handleApplicationRequests