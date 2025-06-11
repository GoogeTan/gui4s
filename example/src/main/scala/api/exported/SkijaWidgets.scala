package me.katze.gui4s.example
package api.exported

import catnip.{BiMonad, FFI}
import cats.{Applicative, FlatMap, Functor, InjectK, Monad}
import cats.data.{ReaderT, StateT}
import cats.effect.ExitCode
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.{CatchEvents, EventReaction, Path}
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.example.place.RunPlacement
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.widget.library.SkijaWidget_

opaque type SkijaUpdate[Event, Value] = EventResult[Event, Value]
type SkijaUpdateT[Event] = SkijaUpdate[Event, *]
opaque type SkijaPlaceInner[IO[_], Value] = StateT[IO, Bounds[Float], Value]
type SkijaPlaceInnerT[IO[_]] = SkijaPlaceInner[IO, *]
type SkijaPlace[IO[_], Value] = SkijaPlaceInner[IO, Sized[Float, Value]]
type SkijaPlaceT[IO[_]] = SkijaPlace[IO, *]

def runSkijaPlace[G[_] : FlatMap, F[_] : Monad, Value](bounds : F[Bounds[Float]], toPlace: SkijaPlace[G, Value])(using I : InjectK[G, F]): F[Value] =
  bounds
    .flatMap(bounds => I(toPlace.run(bounds)))
    .map(_._2.value)
end runSkijaPlace

def getBounds[F[_] : Applicative] : SkijaPlaceInner[F, Bounds[Float]] =
  StateT.get
end getBounds

def setBounds[F[_] : Applicative](bounds : Bounds[Float]) : SkijaPlaceInner[F, Unit] =
  StateT.set(bounds)
end setBounds

given[F[_] : Monad] : Monad[SkijaPlaceInner[F, *]] = summon

def sizeText[F[+_] : Applicative](ffi : FFI[F], text: String, shaper : Shaper, options: SkijaTextStyle):  SkijaPlace[F, SkijaPlacedText] =
  StateT(
    bounds =>
      placeText(ffi = ffi,
        shaper = shaper,
        text = text,
        style = options,
        maxWidth = bounds.horizontal.max
      ).map((placedText) => (bounds, new Sized(placedText.text, placedText.width, placedText.height)))
  )
end sizeText

final class MeasurableRunPlacement[F[_] : Monad, G[+_] : FlatMap](bounds: F[Bounds[Float]])(using I : InjectK[G, F]) extends RunPlacement[F, SkijaPlace[G, *]]:
  override def run[Value](toPlace: SkijaPlace[G, Value]): F[Value] =
    runSkijaPlace(bounds, toPlace)
  end run
end MeasurableRunPlacement

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
type PlacedWidget[F[+_], +Event, -DownEvent] = SkijaWidget_[SkijaUpdateT[Event], SkijaPlace[F, *], SkijaDraw[F, OglWindow], Recomposition[F], DownEvent]
type Widget[F[+_], +Event, -DownEvent] = SkijaPlace[F, PlacedWidget[F, Event, DownEvent]]