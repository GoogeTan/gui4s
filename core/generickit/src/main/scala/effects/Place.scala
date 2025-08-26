package gui4s.core.kit
package effects

import _root_.cats.data.EitherT
import _root_.cats.{Monad, ~>}
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

import scala.reflect.Typeable

type Place[IO[_], Bounds, MeasurementUnit, Error, Value] = OuterPlaceT[IO, Bounds, Error][Sized[MeasurementUnit, Value]]
type PlaceT[IO[_], Bounds, MeasurementUnit, Error] = Place[IO, Bounds, MeasurementUnit, Error, *]

object Place:
  def run[IO[_] : Monad, Bounds, MeasurementUnit, PlaceError](bounds : IO[Bounds]) : PlaceT[IO, Bounds, MeasurementUnit, PlaceError] ~> EitherT[IO, PlaceError, *] =
    new ~>[PlaceT[IO, Bounds, MeasurementUnit, PlaceError], EitherT[IO, PlaceError, *]]:
      override def apply[A](fa : PlaceT[IO, Bounds, MeasurementUnit, PlaceError][A]) : EitherT[IO, PlaceError, A] =
        OuterPlace.run(bounds)(fa.map(_.value))
      end apply
    end new
  end run
  
  def typecheck[
    IO[_] : Monad,
    Bounds, 
    MeasurementUnit, 
    PlaceError,
    U : Typeable
  ](error : (Any, Path) => PlaceError) : [T] => (Any, Path, U => Place[IO, Bounds, MeasurementUnit, PlaceError, T]) => Place[IO, Bounds, MeasurementUnit, PlaceError, T] =
    [T] => (value : Any, path : Path, callback : U => Place[IO, Bounds, MeasurementUnit, PlaceError, T]) =>
      value match
        case v: U => callback(v)
        case _ => OuterPlace.raiseError(error(value, path))
      end match
  end typecheck
end Place

