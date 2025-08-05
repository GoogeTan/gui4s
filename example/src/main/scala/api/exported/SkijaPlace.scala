package me.katze.gui4s.example
package api.exported

import place.{RunPlacement, runPlaceStateT}

import catnip.ForeighFunctionInterface
import catnip.syntax.additional.*
import cats.data.{EitherT, StateT}
import cats.syntax.all.*
import cats.{Applicative, FlatMap, Monad, MonadError, ~>}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.bound.{Bounds, GetBounds, SetBounds}
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}
import scalacache.Cache

opaque type SkijaPlaceInner[IO[_], MeasurementUnit, Error, Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaPlaceInnerT[IO[_], MeasurementUnit, Error] = SkijaPlaceInner[IO, MeasurementUnit, Error, *]
type SkijaPlace[IO[_], MeasurementUnit, Error, Value] = SkijaPlaceInner[IO, MeasurementUnit, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], MeasurementUnit, Error] = SkijaPlace[IO, MeasurementUnit, Error, *]

given skijaInnerMonadIsAnMonadError[IO[_] : Monad, MeasurementUnit, Error] : MonadError[SkijaPlaceInner[IO, MeasurementUnit, Error, *], Error] = summon

def withBounds[IO[_] : Monad, MeasurementUnit, Error, T](original : SkijaPlaceInner[IO, MeasurementUnit, Error, T], bounds : Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) : SkijaPlaceInner[IO, MeasurementUnit, Error, T] =
  for
    initialBounds <- GetBounds.getBoundsStateT[EitherT[IO, Error, *], MeasurementUnit] // Это skijaGetBounds, но почему-то, если его вызвать, а не подставить его тело, то не найдется метод flatMap
    _ <- skijaSetBounds(bounds(initialBounds))
    result <- original
    _ <- skijaSetBounds(initialBounds)
  yield result
end withBounds

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
  GetBounds.getBoundsStateT[EitherT[IO, PlaceError, *], MeasurementUnit]
end skijaGetBounds

def skijaSetBounds[IO[_] : Monad, MeasurementUnit, PlaceError] : SetBounds[SkijaPlaceInner[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
  SetBounds.setBoundsStateT[EitherT[IO, PlaceError, *], MeasurementUnit]
end skijaSetBounds

type SizeText[G[_]] = (text: String, options: SkijaTextStyle) => G[SkijaPlacedText]

def sizeTextStateT[IO[_] : Monad](
                                    ffi : ForeighFunctionInterface[IO],
                                    shaper: Shaper,
                                    cache : Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
                                  ) : SizeText[[Value] =>> StateT[IO, Bounds[Float], Sized[Float, Value]]] =
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
                                              cache : Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
                                            ) : SizeText[SkijaPlace[IO, Float, PlaceError, *]] =
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
