package me.katze.gui4s.example
package api.exported

import catnip.FFI
import cats.Applicative
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}
import scalacache.Cache

import cats.{FlatMap, Monad, ~>}
import cats.syntax.all.*
import catnip.syntax.additional.*
import cats.data.{EitherT, StateT}
import me.katze.gui4s.example.place.RunPlacement
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.bound.Bounds

opaque type SkijaPlaceInner[IO[_], MeasurementUnit, Error, Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaPlaceInnerT[IO[_], MeasurementUnit, Error] = SkijaPlaceInner[IO, MeasurementUnit, Error, *]
type SkijaPlace[IO[_], MeasurementUnit, Error, Value] = SkijaPlaceInner[IO, MeasurementUnit, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], MeasurementUnit, Error] = SkijaPlace[IO, MeasurementUnit, Error, *]

given[F[_] : Monad, MeasurementUnit, Error] : Monad[SkijaPlaceInner[F, MeasurementUnit, Error, *]] = summon

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

type GetBounds[F[_], MeasurementUnit] = F[Bounds[MeasurementUnit]]
type SetBounds[F[_], MeasurementUnit] = Bounds[MeasurementUnit] => F[Unit]

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

type SizeText[F[_], G[_]] = (ffi: FFI[F], text: String, shaper: Shaper, options: SkijaTextStyle) => G[SkijaPlacedText]

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

// TODO replace with mapK
def mapF[F[_] : FlatMap, G[_] : Applicative, U[_], S](f : F ~> G) : (StateT[F, S, *] * U) ~> (StateT[G, S, *] * U) =
  new ~>[StateT[F, S, *] * U,  StateT[G, S, *] * U]:
    override def apply[A](fa: StateT[F, S, U[A]]): StateT[G, S, U[A]] =
      StateT(state =>
        f(fa.run(state))
      )
    end apply
  end new
end mapF
