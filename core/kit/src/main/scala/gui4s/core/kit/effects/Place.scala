package gui4s.core.kit
package effects

import scala.reflect.Typeable

import catnip.syntax.all._
import cats._

import gui4s.core.layout.Sized
import gui4s.core.widget.Path

type Place[IO[_], Bounds, Size, Value] = PlacementEffect[IO, Bounds, Sized[Size, Value]]
type PlaceC[IO[_], Bounds, Size] = Place[IO, Bounds, Size, *]

object Place:
  def typecheck[
    PlacementEffect[_] : MonadErrorC[PlaceError],
    Situated[_],
    PlaceError,
    U : Typeable
  ](error : (Any, Path) => PlaceError) : [T] => (Any, Path, U => PlacementEffect[Situated[T]]) => PlacementEffect[Situated[T]] =
    [T] => (value : Any, path : Path, callback : U => PlacementEffect[Situated[T]]) =>
      value match
        case v: U => callback(v)
        case _ => MonadError[PlacementEffect, PlaceError].raiseError(error(value, path))
      end match
  end typecheck

  def addNameToPath[IO[_] : Monad, Bounds, MeasurementUnit, Error](name : String)
    : PlaceC[IO, Bounds, MeasurementUnit] ~> PlaceC[IO, Bounds, MeasurementUnit] =
    new (PlaceC[IO, Bounds, MeasurementUnit] ~> PlaceC[IO, Bounds, MeasurementUnit]):
      override def apply[A](p: Place[IO, Bounds, MeasurementUnit, A]) : Place[IO, Bounds, MeasurementUnit, A] =
        PlacementEffect.addNameToPath[IO, Bounds](name)[Sized[MeasurementUnit, A]](p)
      end apply
    end new
  end addNameToPath
end Place

