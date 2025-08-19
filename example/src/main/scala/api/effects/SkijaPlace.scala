package me.katze.gui4s.example
package api.effects

import api.widget.{SizeText, TextCache, sizeTextFFI}

import _root_.cats.data.{EitherT, StateT}
import _root_.cats.syntax.all.*
import _root_.cats.{FlatMap, Monad, MonadError, ~>}
import catnip.*
import catnip.syntax.additional.*
import catnip.syntax.functionk.runStateT
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.geometry.{InfinityOr, Rect}
import me.katze.gui4s.layout.Sized

opaque type SkijaOuterPlace[IO[_], Bounds, Error, Value] = StateT[EitherT[IO, Error, *], Bounds, Value]
type SkijaOuterPlaceT[IO[_], BoundUnit, Error] = SkijaOuterPlace[IO, BoundUnit, Error, *]
type SkijaPlace[IO[_], Bounds, MeasurementUnit, Error, Value] = SkijaOuterPlace[IO, Bounds, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], Bounds, MeasurementUnit, Error] = SkijaPlace[IO, Bounds, MeasurementUnit, Error, *]

object SkijaOuterPlace:
  def liftF[IO[_] : Monad, Bounds, Error, Value](value : IO[Value]) : SkijaOuterPlace[IO, Bounds, Error, Value] =
    liftK(value)
  end liftF
  
  def withBounds[IO[_] : Monad, Bounds, Error, T](original : SkijaOuterPlace[IO, Bounds, Error, T], bounds : Bounds => Bounds) : SkijaOuterPlace[IO, Bounds, Error, T] =
    for
      initialBounds <- Get.stateT[EitherT[IO, Error, *], Bounds] // Это skijaGetBounds, но почему-то, если его вызвать, а не подставить его тело, то не найдется метод flatMap
      _ <- setBounds(bounds(initialBounds))
      result <- original
      _ <- setBounds(initialBounds)
    yield result
  end withBounds

  given monadInstance[IO[_] : Monad, Bounds, Error] : MonadError[SkijaOuterPlace[IO, Bounds, Error, *], Error] = summon

  def raiseError[IO[_] : Monad, Bounds, PlaceError, Value](error : => PlaceError) : SkijaOuterPlace[IO, Bounds, PlaceError, Value] =
    StateT.liftF(EitherT.left(error.pure))
  end raiseError

  def run[IO[_] : Monad, Bounds, PlaceError](bounds : IO[Bounds]) : SkijaOuterPlaceT[IO, Bounds, PlaceError] ~> EitherT[IO, PlaceError, *] =
    runStateT[EitherT[IO, PlaceError, *], Bounds](EitherT.liftF(bounds))
  end run

  def getBounds[IO[_] : Monad, Bounds, PlaceError]: Get[SkijaOuterPlace[IO, Bounds, PlaceError, *], Bounds] =
    Get.stateT[EitherT[IO, PlaceError, *], Bounds]
  end getBounds

  def setBounds[IO[_] : Monad, Bounds, PlaceError]: Set[SkijaOuterPlace[IO, Bounds, PlaceError, *], Bounds] =
    Set.stateT[EitherT[IO, PlaceError, *], Bounds]
  end setBounds
  
  def liftK[IO[_] : Monad, Bounds, PlaceError] : IO ~> SkijaOuterPlaceT[IO, Bounds, PlaceError] =
    EitherT.liftK.andThen(StateT.liftK)
  end liftK
end SkijaOuterPlace

object SkijaPlace:
  def run[IO[_] : Monad, Bounds, MeasurementUnit, PlaceError](bounds : IO[Bounds]) : SkijaPlaceT[IO, Bounds, MeasurementUnit, PlaceError] ~> EitherT[IO, PlaceError, *] =
    new ~>[SkijaPlaceT[IO, Bounds, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]]:
      override def apply[A](fa : SkijaPlaceT[IO, Bounds, MeasurementUnit, PlaceError][A]) : EitherT[IO, PlaceError, A] =
        SkijaOuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText[
    IO[_] : Monad,
    Bounds,
    PlaceError
  ](
    ffi : ForeighFunctionInterface[IO],
    shaper : Shaper,
    cache : TextCache[IO],
    widthFromBounds : Bounds => Option[Float],
  ) : SizeText[SkijaPlace[IO, Bounds, Float, PlaceError, *]] =
    sizeTextFFI[IO, SkijaOuterPlaceT[IO, Bounds, PlaceError]](
      SkijaOuterPlace.getBounds[IO, Bounds, PlaceError].map(widthFromBounds),
      ffi,
      shaper,
      cache,
      SkijaOuterPlace.liftK,
    )
  end sizeText
end SkijaPlace

