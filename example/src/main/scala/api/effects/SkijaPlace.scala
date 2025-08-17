package me.katze.gui4s.example
package api.effects

import api.widget.{SizeText, TextCache, sizeTextFFI}
import place.{RunPlacement, runPlaceStateT}

import _root_.cats.data.{EitherT, StateT}
import _root_.cats.syntax.all.*
import _root_.cats.{FlatMap, Monad, MonadError, ~>}
import catnip.*
import catnip.syntax.additional.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.bound.{Bounds, GetBounds, SetBounds}

opaque type SkijaOuterPlace[IO[_], MeasurementUnit, Error, Value] = StateT[EitherT[IO, Error, *], Bounds[MeasurementUnit], Value]
type SkijaOuterPlaceT[IO[_], MeasurementUnit, Error] = SkijaOuterPlace[IO, MeasurementUnit, Error, *]
type SkijaPlace[IO[_], MeasurementUnit, Error, Value] = SkijaOuterPlace[IO, MeasurementUnit, Error, Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], MeasurementUnit, Error] = SkijaPlace[IO, MeasurementUnit, Error, *]

object SkijaOuterPlace:
  def liftF[IO[_] : Monad, MeasurementUnit, Error, Value](value : IO[Value]) : SkijaOuterPlace[IO, MeasurementUnit, Error, Value] =
    liftK(value)
  end liftF
  
  def withBounds[IO[_] : Monad, MeasurementUnit, Error, T](original : SkijaOuterPlace[IO, MeasurementUnit, Error, T], bounds : Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) : SkijaOuterPlace[IO, MeasurementUnit, Error, T] =
    for
      initialBounds <- Get.stateT[EitherT[IO, Error, *], Bounds[MeasurementUnit]] // Это skijaGetBounds, но почему-то, если его вызвать, а не подставить его тело, то не найдется метод flatMap
      _ <- setBounds(bounds(initialBounds))
      result <- original
      _ <- setBounds(initialBounds)
    yield result
  end withBounds

  given monadInstance[IO[_] : Monad, MeasurementUnit, Error] : MonadError[SkijaOuterPlace[IO, MeasurementUnit, Error, *], Error] = summon

  def raiseError[IO[_] : Monad, MeasurementUnit, PlaceError, Value](error : => PlaceError) : SkijaOuterPlace[IO, MeasurementUnit, PlaceError, Value] =
    StateT.liftF(EitherT.left(error.pure))
  end raiseError

  def run[IO[_] : Monad, MeasurementUnit, PlaceError](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaOuterPlaceT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]] =
    runPlaceStateT[EitherT[IO, PlaceError, *], MeasurementUnit](EitherT.liftF(bounds))
  end run

  def getBounds[IO[_] : Monad, MeasurementUnit, PlaceError]: GetBounds[SkijaOuterPlace[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
    Get.stateT[EitherT[IO, PlaceError, *], Bounds[MeasurementUnit]]
  end getBounds

  def setBounds[IO[_] : Monad, MeasurementUnit, PlaceError]: SetBounds[SkijaOuterPlace[IO, MeasurementUnit, PlaceError, *], MeasurementUnit] =
    Set.stateT[EitherT[IO, PlaceError, *], Bounds[MeasurementUnit]]
  end setBounds
  
  def liftK[IO[_] : Monad, MeasurementUnit, PlaceError] : IO ~> SkijaOuterPlaceT[IO, MeasurementUnit, PlaceError] =
    EitherT.liftK.andThen(StateT.liftK)
  end liftK
end SkijaOuterPlace

object SkijaPlace:
  def run[IO[_] : Monad, MeasurementUnit, PlaceError](bounds : IO[Bounds[MeasurementUnit]]) : RunPlacement[SkijaPlaceT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]] =
    new ~>[SkijaPlaceT[IO, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]]:
      override def apply[A](fa : SkijaPlaceT[IO, MeasurementUnit, PlaceError][A]) : EitherT[IO, PlaceError, A] =
        SkijaOuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText[IO[_] : Monad, PlaceError](
                                            ffi : ForeighFunctionInterface[IO],
                                            shaper : Shaper,
                                            cache : TextCache[IO] 
                                          ) : SizeText[SkijaPlace[IO, Float, PlaceError, *]] =
    sizeTextFFI[IO, SkijaOuterPlaceT[IO, Float, PlaceError]](
      SkijaOuterPlace.getBounds,
      ffi,
      shaper,
      cache,
      SkijaOuterPlace.liftK,
    )
  end sizeText
end SkijaPlace

