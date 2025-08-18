package me.katze.gui4s.example
package api.widget

import catnip.{ForeighFunctionInterface, Zip}
import catnip.syntax.all.given
import cats.{Applicative, Monad, Order, Traverse}
import me.katze.gui4s.example.{*, given}
import api.effects.{*, given}

import cats.kernel.Monoid
import me.katze.gui4s.geometry.{Axis, Point3d, Rect}
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.{SkijaDraw, drawAt}
import me.katze.gui4s.widget.library.*

def skijaLinearContainer[
  IO[_] : {Monad},
  PlacedWidget,
  BoundUnit,
  MeasurementUnit : Fractional,
  PlaceError,
  Container[_] : {Applicative, Traverse, Zip}
](
  container : ContainerWidget[PlacedWidget, Container, SkijaPlaceT[IO, Rect[BoundUnit], MeasurementUnit, PlaceError], Point3d[MeasurementUnit]],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit
) : LinearContainer[
  SkijaPlace[IO, Rect[BoundUnit], MeasurementUnit, PlaceError, PlacedWidget],
  SkijaOuterPlaceT[IO, Rect[BoundUnit], PlaceError],
  Container,
  BoundUnit,
  MeasurementUnit,
  Axis
] =
  linearContainer[
    PlacedWidget,
    SkijaOuterPlaceT[IO, Rect[BoundUnit], PlaceError],
    Container,
    BoundUnit,
    MeasurementUnit,
  ](
    container = container,
    getBounds = SkijaOuterPlace.getBounds,
    setBounds = SkijaOuterPlace.setBounds,
    cut = cut
  )
end skijaLinearContainer

def skijaContainer[
  IO[_] : Monad,
  Clip,
  UpdateError,
  PlaceError,
  Event,
  DownEvent,
  RecompositionReaction : Monoid,
  Container[_] : {Traverse},
  Bounds,
](
  ffi : ForeighFunctionInterface[IO],
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => SkijaUpdate[IO, Float, Clip, UpdateError, Event, Container[B]]) => SkijaUpdate[IO, Float, Clip, UpdateError, Event, Container[B]]
) : ContainerWidget[
  Widget[
        SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
        SkijaPlaceT[IO, Bounds, Float, PlaceError],
        SkijaDraw[IO],
        RecompositionReaction,
        DownEvent,
  ],
  Container,
  SkijaPlaceT[IO, Bounds, Float, PlaceError],
  Point3d[Float]
] =
  given Order[Point3d[Float]] = Order.by(_.z)
  container(
    (draw, meta) => drawAt(ffi, draw, meta.x, meta.y),
    [T] => (update, point) => SkijaUpdate.withCoordinates(update)(_ + point),
    SkijaUpdate.isEventHandled[IO, Float, Clip, UpdateError, Event],
    updateListOrdered
  )
end skijaContainer
