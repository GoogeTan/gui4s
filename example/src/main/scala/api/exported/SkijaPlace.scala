package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import cats.{Applicative, FlatMap, Monad, MonadError, ~>}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.skija.{Pixel, SkijaPlacedText, SkijaTextStyle, placeText}
import scalacache.Cache
import cats.syntax.all.*
import catnip.syntax.additional.*
import cats.data.{EitherT, StateT}
import me.katze.gui4s.example.impl.{GetBounds, SetBounds, getBoundsStateT, setBoundsStateT}
import me.katze.gui4s.example.place.{RunPlacement, runPlaceStateT}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.bound.Bounds

opaque type SkijaPlaceInner[IO[_], MeasurementUnit, Error, Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaPlaceInnerT[IO[_], MeasurementUnit, Error] = SkijaPlaceInner[IO, MeasurementUnit, Error, *]
type SkijaPlace[IO[_], MeasurementUnit, Error, Value] = SkijaPlaceInner[IO, MeasurementUnit, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], MeasurementUnit, Error] = SkijaPlace[IO, MeasurementUnit, Error, *]

given skijaInnerMonadIsAnMonadError[F[_] : Monad, MeasurementUnit, Error] : MonadError[SkijaPlaceInner[F, MeasurementUnit, Error, *], Error] = summon

def raiseError[IO[_] : Monad, MeasurementUnit, PlaceError, Value](error : => PlaceError) : SkijaPlaceInner[IO, MeasurementUnit, PlaceError, Value] =
  StateT.liftF(EitherT.left(error.pure))
end raiseError

def skijaInnerRunPlace[IO[_] : Monad, MeasurementUnit, PlaceError](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceInnerT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]] =
  runPlaceStateT[EitherT[IO, PlaceError, *], MeasurementUnit](EitherT.liftF(bounds))
end skijaInnerRunPlace

def skijaRunPlace[IO[_] : Monad, MeasurementUnit, PlaceError](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]] =
  [Value] => (value : SkijaPlaceInner[IO, MeasurementUnit, PlaceError, Sized[MeasurementUnit, Value]]) =>
    skijaInnerRunPlace(bounds)(value.map(_.value))
end skijaRunPlace

def skijaGetBounds[IO[_] : Monad, MeasurementUnit, PlaceError] : GetBounds[SkijaPlaceInner[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
  getBoundsStateT[EitherT[IO, PlaceError, *], MeasurementUnit]
end skijaGetBounds

def skijaSetBounds[IO[_] : Monad, MeasurementUnit, PlaceError] : SetBounds[SkijaPlaceInner[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
  setBoundsStateT[EitherT[IO, PlaceError, *], MeasurementUnit]
end skijaSetBounds

type SizeText[G[_]] = (text: String, options: SkijaTextStyle) => G[SkijaPlacedText]

def sizeTextStateT[IO[_] : Monad](
                                    ffi : ForeighFunctionInterface[IO],
                                    shaper: Shaper,
                                    cache : Cache[IO, (String, SkijaTextStyle, Option[Pixel]), Sized[Pixel, SkijaPlacedText]]
                                  ) : SizeText[[Value] =>> StateT[IO, Bounds[Pixel], Sized[Pixel, Value]]] =
  (text: String, options: SkijaTextStyle) =>
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

def sizeTextLift[F[_], G[_]](original : SizeText[F], inj : F ~> G) : SizeText[G] =
  (text, options) =>
    inj(original(text, options))
end sizeTextLift

def skijaSizeText[IO[_] : Monad, PlaceError](
                                              ffi : ForeighFunctionInterface[IO],
                                              shaper : Shaper,
                                              cache : Cache[IO, (String, SkijaTextStyle, Option[Pixel]), Sized[Pixel, SkijaPlacedText]]
                                            ) : SizeText[SkijaPlace[IO, Pixel, PlaceError, *]] =
  sizeTextLift(sizeTextStateT(ffi, shaper, cache), mapF(EitherT.liftK[IO, PlaceError]))
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
