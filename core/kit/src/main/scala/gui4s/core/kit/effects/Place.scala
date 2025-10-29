package gui4s.core.kit
package effects

import _root_.cats.MonadError
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
end Place

