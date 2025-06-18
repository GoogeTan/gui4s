package me.katze.gui4s.example
package api.exported

import catnip.{BiMonad, FFI}
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
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.widget.library.Widget_
import sun.jvm.hotspot.runtime.PerfMemory.end

opaque type SkijaUpdate[Event, Value] = EventResult[Event, Value]
type SkijaUpdateT[Event] = SkijaUpdate[Event, *]
opaque type SkijaPlaceInner[IO[+_], +Error, MeasurementUnit, +  Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaPlaceInnerT[IO[_], Error, MeasurementUnit] = SkijaPlaceInner[IO, Error, MeasurementUnit, *]
type SkijaPlace[IO[_], Error, MeasurementUnit, +Value] = SkijaPlaceInner[IO, Error, MeasurementUnit, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], Error, MeasurementUnit] = SkijaPlace[IO, Error, MeasurementUnit, *]

def runPlaceStateT[IO[_] : FlatMap, MeasurementUnit](
                                                      bounds : IO[Bounds[MeasurementUnit]]
                                                    ) : RunPlacement[StateT[IO, Bounds[MeasurementUnit], *], IO] =
  [Value] => (toPlace : StateT[IO, Bounds[MeasurementUnit], Value]) =>
    bounds
      .flatMap(bounds => toPlace.run(bounds))
      .map(_._2)
end runPlaceStateT

def skijaInnerRunPlace[IO[_] : Monad, PlaceError, MeasurementUnit](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceInnerT[IO, PlaceError, MeasurementUnit], EitherT[IO, PlaceError, *]] =
  runPlaceStateT[EitherT[IO, PlaceError, *], MeasurementUnit](EitherT.liftF(bounds))
end skijaInnerRunPlace

def skijaRunPlace[IO[_] : Monad, PlaceError, MeasurementUnit](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceT[IO, PlaceError, MeasurementUnit], EitherT[IO, PlaceError, *]] =
  [Value] => (value : SkijaPlaceInner[IO, PlaceError, MeasurementUnit, Sized[MeasurementUnit, Value]]) =>
    skijaInnerRunPlace(bounds)(value.map(_.value))
end skijaRunPlace

def runPlaceLift[U[_], F[_], G[_]](original : RunPlacement[F, U], inj : G ~> F) : RunPlacement[G, U] =
  [Value] => (value : G[Value]) => original(inj(value))
end runPlaceLift

def raiseError[IO[_] : Monad, Error, MeasurementUnit](error : Error) : SkijaPlaceInner[IO, Error, MeasurementUnit, Nothing] =
  StateT.liftF(EitherT.left(error.pure[IO]))
end raiseError

type GetBounds[F[_], MeasurementUnit] = F[Bounds[MeasurementUnit]]
type SetBounds[F[_], MeasurementUnit] = Bounds[MeasurementUnit] => F[Unit]

def getBoundsStateT[F[_] : Applicative, MeasurementUnit] : GetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
  StateT.get
end getBoundsStateT

def setBoundsStateT[F[_] : Applicative, MeasurementUnit] : SetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
  StateT.set
end setBoundsStateT

def skijaGetBounds[IO[_] : Monad, MeasurementUnit] : GetBounds[SkijaPlaceInner[IO, Nothing, MeasurementUnit, *], MeasurementUnit] =
  getBoundsStateT[EitherT[IO, Nothing, *], MeasurementUnit]
end skijaGetBounds

def skijaSetBounds[IO[_] : Monad, MeasurementUnit] : SetBounds[SkijaPlaceInner[IO, Nothing, MeasurementUnit, *], MeasurementUnit] =
  setBoundsStateT[EitherT[IO, Nothing, *], MeasurementUnit]
end skijaSetBounds

def getBounds[F[_], G[_], MeasurementUnit](original : GetBounds[F, MeasurementUnit], inj : F ~> G) : GetBounds[G, MeasurementUnit] =
  inj(original)
end getBounds

def setBounds[F[_], G[_], MeasurementUnit](original : SetBounds[F, MeasurementUnit], inj : F ~> G) : SetBounds[G, MeasurementUnit] =
  bounds => inj(original(bounds))
end setBounds

given[F[_] : Monad, Error, MeasurementUnit] : Monad[SkijaPlaceInner[F, Error, MeasurementUnit, *]] = summon

type SizeText[F[_], G[_]] = (ffi : FFI[F], text : String, shaper : Shaper, options : SkijaTextStyle) => G[SkijaPlacedText]

def sizeTextStateT[F[+_] : Applicative] : SizeText[F, [Value] =>> StateT[F, Bounds[Float], Sized[Float, Value]]] =
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

given BiMonad[SkijaUpdate] = summon
given CatchEvents[SkijaUpdate] = summon


def runEventReaction[T, Event](reaction : EventReaction[T, Event, ?], path : Path) : SkijaUpdate[Event, T] =
  StateT(isEventHandled => EventResult_((isEventHandled, reaction.newState), reaction.parentEvent))
end runEventReaction

def handleApplicationRequests[F[_] : Monad] : [T] => SkijaUpdateT[ApplicationRequest][T] => F[Either[ExitCode, T]] =
  [T] => update =>
    val reaction : EventResult_[ApplicationRequest, (Boolean, T)] = update.run(false)
    reaction.events.foldM(Right(reaction.widget._2))((_, request) =>
      request match
        case ApplicationRequest.CloseApp(code) => Left(code).pure[F]
    )
end handleApplicationRequests

type Recomposition[F[_]] = F[Unit]
type PlacedWidget[F[+_], MeasurementUnit, +PlaceError, +Event, -DownEvent] = Widget_[SkijaUpdateT[Event], SkijaPlace[F, PlaceError, MeasurementUnit, *], SkijaDraw[F, OglWindow], Recomposition[F], DownEvent]
type Widget[F[+_], MeasurementUnit, +PlaceError, +Event, -DownEvent] = SkijaPlace[F, PlaceError, MeasurementUnit, PlacedWidget[F, MeasurementUnit, PlaceError, Event, DownEvent]]