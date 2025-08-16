package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.{Applicative, Monad, Order, SemigroupK, Traverse}
import cats.syntax.all.*
import me.katze.gui4s.example.{*, given}
import api.effects.{*, given}
import api.given

import cats.kernel.Monoid
import me.katze.gui4s.example.app.{SkijaPlacedWidget, SkijaWidget}
import me.katze.gui4s.geometry.{Axis, Point3d}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement, rowColumnLayoutPlacement}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.drawAt
import me.katze.gui4s.widget.library.{ContainerWidget, Widget, container, *}
import me.katze.gui4s.widget.handle.Layout

import scala.language.experimental.namedTypeArguments

def skijaLayout[
  F[+_] : {Monad, ForeighFunctionInterface as ffi},
  PlacedWidget,
  MeasurementUnit : Fractional,
  PlaceError,
  Container[_] : {Applicative, Traverse}
](
  container : ContainerWidget[PlacedWidget, Container, SkijaPlaceT[F, MeasurementUnit, PlaceError], Point3d[MeasurementUnit]],
  zip : [A, B] => (Container[A], Container[B]) => Container[(A, B)]
) : LinearLayout[
  SkijaPlace[F, MeasurementUnit, PlaceError, PlacedWidget],
  SkijaOuterPlaceT[F, MeasurementUnit, PlaceError],
  Container,
  MeasurementUnit,
  Axis
] =
  linearLayout[
    PlacedWidget,
    SkijaOuterPlaceT[F, MeasurementUnit, PlaceError],
    Container,
    MeasurementUnit,
  ](
    container = container,
    getBounds = SkijaOuterPlace.getBounds,
    setBounds = SkijaOuterPlace.setBounds,
    zip = zip
  )
end skijaLayout

def skijaContainer[
  F[_] : Monad,
  Clip,
  UpdateError,
  PlaceError,
  Event,
  DownEvent,
  Container[_] : {Traverse}
](
  ffi : ForeighFunctionInterface[F],
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => SkijaUpdate[F, Float, Clip, UpdateError, Event, Container[B]]) => SkijaUpdate[F, Float, Clip, UpdateError, Event, Container[B]]
) : ContainerWidget[
  SkijaPlacedWidget[F, Float, Clip, UpdateError, PlaceError, Event, DownEvent],
  Container,
  SkijaPlaceT[F, Float, PlaceError],
  Point3d[Float]
] =
  given Order[Point3d[Float]] = Order.by(_.z)
  container(
    (draw, meta) => drawAt(ffi, draw, meta.x, meta.y),
    [T] => (update, point) => SkijaUpdate.withCoordinates(update)(_ + point),
    SkijaUpdate.isEventHandled[F, Float, Clip, UpdateError, Event],
    updateListOrdered
  )
end skijaContainer
