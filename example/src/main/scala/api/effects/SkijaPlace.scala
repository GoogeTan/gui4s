package me.katze.gui4s.example
package api.effects

import api.widget.{SizeText, TextCache, sizeTextFFI}

import _root_.cats.data.{EitherT, StateT}
import _root_.cats.syntax.all.*
import _root_.cats.{Monad, MonadError, ~>}
import catnip.*
import catnip.syntax.all.{*, given}
import catnip.syntax.functionk.runStateT
import catnip.transformer.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.widget.Path

import scala.reflect.Typeable

type SkijaPlace[IO[_], Bounds, MeasurementUnit, Error, Value] = SkijaOuterPlaceT[IO, Bounds, Error][Sized[MeasurementUnit, Value]]
type SkijaPlaceT[IO[_], Bounds, MeasurementUnit, Error] = SkijaPlace[IO, Bounds, MeasurementUnit, Error, *]

object SkijaPlace:
  def run[IO[_] : Monad, Bounds, MeasurementUnit, PlaceError](bounds : IO[Bounds]) : SkijaPlaceT[IO, Bounds, MeasurementUnit, PlaceError] ~> EitherT[IO, PlaceError, *] =
    new ~>[SkijaPlaceT[IO, Bounds, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]]:
      override def apply[A](fa : SkijaPlaceT[IO, Bounds, MeasurementUnit, PlaceError][A]) : EitherT[IO, PlaceError, A] =
        SkijaOuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def sizeText[
    IO[_] : Monad as IOM,
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
    )(using IOM, SkijaOuterPlace.monadInstance)
  end sizeText
  
  def typecheck[
    IO[_] : Monad,
    Bounds, 
    MeasurementUnit, 
    PlaceError,
    U : Typeable
  ](error : (Any, Path) => PlaceError) : [T] => (Any, Path, U => SkijaPlace[IO, Bounds, MeasurementUnit, PlaceError, T]) => SkijaPlace[IO, Bounds, MeasurementUnit, PlaceError, T] =
    [T] => (value : Any, path : Path, callback : U => SkijaPlace[IO, Bounds, MeasurementUnit, PlaceError, T]) =>
      value match
        case v: U => callback(v)
        case _ => SkijaOuterPlace.raiseError(error(value, path))
      end match
  end typecheck
end SkijaPlace

