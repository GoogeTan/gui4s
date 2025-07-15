package me.katze.gui4s.example
package api.exported

import catnip.{BiMonad, FFI}
import catnip.syntax.all.{*, given}
import cats.arrow.FunctionK
import cats.{Applicative, Apply, FlatMap, Functor, InjectK, Monad, Semigroup, ~>}
import cats.data.{EitherT, ReaderT, StateT, WriterT}
import cats.effect.{ExitCode, Sync}
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.{CatchEvents, EventReaction, Path, given}
import cats.syntax.all.*
import io.github.humbleui.skija.PathMeasure
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.example.place.RunPlacement
import me.katze.gui4s.example.update.ApplicationRequest
import me.katze.gui4s.layout.{Point3d, Sized, given}
import me.katze.gui4s.widget.library.Widget_
import scalacache.Cache
import scalacache.caffeine.CaffeineCache
import sun.jvm.hotspot.runtime.PerfMemory.end

opaque type SkijaUpdate[IO[_], MeasurementUnit, Event, Value] = EventResult[IO, MeasurementUnit, Event, Value]
type SkijaUpdateT[IO[_], MeasurementUnit, Event] = SkijaUpdate[IO, MeasurementUnit, Event, *]

def liftIOToSkijaUpdate[IO[_] : Monad, MeasurementUnit, Event, Value](io : IO[Value]) : SkijaUpdate[IO, MeasurementUnit, Event, Value] =
  StateT.liftF(WriterT.liftF(io))
end liftIOToSkijaUpdate

opaque type SkijaPlaceInner[IO[_], MeasurementUnit, Error, Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaPlaceInnerT[IO[_], MeasurementUnit, Error] = SkijaPlaceInner[IO, MeasurementUnit, Error, *]
type SkijaPlace[IO[_], MeasurementUnit, Error, Value] = SkijaPlaceInner[IO, MeasurementUnit, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], MeasurementUnit, Error] = SkijaPlace[IO, MeasurementUnit, Error, *]

type SkijaRecomposition[F[_]] = F[Unit]

type SkijaPlacedWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = Widget_[SkijaUpdateT[F, MeasurementUnit, Event], SkijaPlaceT[F, MeasurementUnit, PlaceError], SkijaDraw[F, OglWindow], SkijaRecomposition[F], DownEvent]
type SkijaWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = SkijaPlace[F, MeasurementUnit, PlaceError, SkijaPlacedWidget[F, MeasurementUnit, PlaceError, Event, DownEvent]]

type GetBounds[F[_], MeasurementUnit] = F[Bounds[MeasurementUnit]]
type SetBounds[F[_], MeasurementUnit] = Bounds[MeasurementUnit] => F[Unit]
type SizeText[F[_], G[_]] = (ffi: FFI[F], text: String, shaper: Shaper, options: SkijaTextStyle) => G[SkijaPlacedText]

given[IO[_] : Monad, MeasurementUnit] : BiMonad[SkijaUpdate[IO, MeasurementUnit, *, *]] = stateWrapsBiMonad[[A, B] =>> WriterT[IO, List[A], B], EventResultState[MeasurementUnit]](using writerIsBiMonad)

def markEventHandled[IO[_] : Applicative, MeasurementUnit, Event] : SkijaUpdate[IO, MeasurementUnit, Event, Unit] =
  StateT.modify(me.katze.gui4s.example.markEventHandled)
end markEventHandled

def getCoordinates[IO[_] : Applicative, MeasurementUnit, Event] : SkijaUpdate[IO, MeasurementUnit, Event, Point3d[MeasurementUnit]] =
  StateT.get[WriterT[IO, List[Event], *], EventResultState[MeasurementUnit]].map(_.widgetCoordinates)
end getCoordinates

def addCoordinates[IO[_] : Applicative, MeasurementUnit : Numeric, Event](coordinates : Point3d[MeasurementUnit]) : SkijaUpdate[IO, MeasurementUnit, Event, Unit] =
  StateT.modify(_.addCoordinates(coordinates))
end addCoordinates

def raiseEvents[IO[_] : Applicative, MeasurementUnit, Event](events : List[Event]) : SkijaUpdate[IO, MeasurementUnit, Event, Unit] =
  StateT.liftF(WriterT.tell(events))
end raiseEvents

def mapEvents[IO[_] : Monad, MeasurementUnit, Event1, Event2, T](f : Event1 => Event2)(skijaUpdate : SkijaUpdate[IO, MeasurementUnit, Event1, T]) : SkijaUpdate[IO, MeasurementUnit, Event2, T] =
  skijaUpdate.catchEvents.flatMap((newEvents, value) => raiseEvents(newEvents.map(f)).as(value))
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


def sizeTextStateT[IO[_] : Monad](
                                  cache : Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
                                ) : SizeText[IO, [Value] =>> StateT[IO, Bounds[Float], Sized[Float, Value]]] =
  (ffi : FFI[IO], text: String, shaper : Shaper, options: SkijaTextStyle) =>
    StateT(
      bounds =>
        cache.cachingF(
          (text, options, bounds.horizontal.max)
        )(None)(
          placeText(ffi = ffi,
            shaper = shaper,
            text = text,
            style = options,
            maxWidth = bounds.horizontal.max
          ).map(placedText => new Sized(placedText.text, placedText.width, placedText.height))
        ).map(placedText => (bounds, placedText))
    )
end sizeTextStateT

def sizeTextLift[U[_], F[_], G[_]](original : SizeText[U, F], inj : F ~> G) : SizeText[U, G] =
  (ffi, text, shaper, options) =>
    inj(original(ffi, text, shaper, options))
end sizeTextLift

def skijaSizeText[IO[_] : Monad, PlaceError](
                                              cache : Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
                                            ) : SizeText[IO, SkijaPlace[IO, Float, PlaceError, *]] =
  sizeTextLift(sizeTextStateT(cache), mapF(EitherT.liftK[IO, PlaceError]))
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

given[IO[_] : Monad, MeasurementUnit] : CatchEvents[SkijaUpdate[IO, MeasurementUnit, *, *]] = liftStateTCatchEvents[[A, B] =>> WriterT[IO, List[A], B], EventResultState[MeasurementUnit]](using writerIsBiMonad)

def runEventReaction[IO[_] : Monad, MeasurementUnit, T, Event](reaction : EventReaction[T, Event, ?], path : Path) : SkijaUpdate[IO, MeasurementUnit, Event, T] =
  StateT(isEventHandled => WriterT.tell(reaction.parentEvent).as((isEventHandled, reaction.newState)))
end runEventReaction

def handleApplicationRequests[IO[_] : Monad, MeasurementUnit : Numeric as N] : [T] => SkijaUpdate[IO, MeasurementUnit, ApplicationRequest, T] => IO[Either[ExitCode, T]] =
  [T] => update =>
    update.run(emptyEventResultState).run.flatMap(result =>
      val (events, (_, widget)) = result
      events.foldM[IO, Either[ExitCode, T]](Right(widget))((_, request) =>
        request match
          case ApplicationRequest.CloseApp(code) => Left(code).pure[IO]
      )
    )
end handleApplicationRequests