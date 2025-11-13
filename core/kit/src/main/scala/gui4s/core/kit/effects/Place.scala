package gui4s.core.kit
package effects

import cats.*
import catnip.syntax.all.*
import gui4s.core.layout.Sized
import gui4s.core.widget.Path

import scala.reflect.Typeable

type Place[IO[_], Bounds, MeasurementUnit, Error, Value] = OuterPlace[IO, Bounds, Error, Sized[MeasurementUnit, Value]]
type PlaceC[IO[_], Bounds, MeasurementUnit, Error] = Place[IO, Bounds, MeasurementUnit, Error, *]

object Place:
  def typecheck[
    OuterPlace[_] : MonadErrorC[PlaceError],
    InnerPlace[_],
    PlaceError,
    U : Typeable
  ](error : (Any, Path) => PlaceError) : [T] => (Any, Path, U => OuterPlace[InnerPlace[T]]) => OuterPlace[InnerPlace[T]] =
    [T] => (value : Any, path : Path, callback : U => OuterPlace[InnerPlace[T]]) =>
      value match
        case v: U => callback(v)
        case _ => MonadError[OuterPlace, PlaceError].raiseError(error(value, path))
      end match
  end typecheck

  def addNameToPath[IO[_] : Monad, Bounds, MeasurementUnit, Error](name : String)
    : PlaceC[IO, Bounds, MeasurementUnit, Error] ~> PlaceC[IO, Bounds, MeasurementUnit, Error] =
    new (PlaceC[IO, Bounds, MeasurementUnit, Error] ~> PlaceC[IO, Bounds, MeasurementUnit, Error]):
      override def apply[A](p: Place[IO, Bounds, MeasurementUnit, Error, A]) : Place[IO, Bounds, MeasurementUnit, Error, A] =
        OuterPlace.addNameToPath[IO, Bounds, Error](name)[Sized[MeasurementUnit, A]](p)
      end apply
    end new
  end addNameToPath
end Place

