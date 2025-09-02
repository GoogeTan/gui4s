package gui4s.desktop.kit
package widgets

import effects.*
import effects.Update.given
import effects.OuterPlace.given
import effects.Place.given
import effects.Draw.given

import catnip.effect.SyncForeignFunctionInterface
import catnip.syntax.list.orderedListProcessing
import catnip.syntax.zip.given
import catnip.syntax.applicative.given
import catnip.ForeignFunctionInterface
import cats.*
import cats.data.*
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, InfinityOr, Point3d}
import gui4s.desktop.widget.library.{ContainerWidget, LinearContainer, container as genericContainer, linearContainer as genericLinearContainer}
import gui4s.desktop.skija.drawAt

def container[
  IO[_] : {Monad, ForeignFunctionInterface as ffi},
  Container[_] : Traverse,
  Event
](
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[IO, Event, Container[B]]) => Update[IO, Event, Container[B]]
) : ContainerWidget[DesktopPlacedWidget[IO, Event], Container, PlaceC[IO], Point3d[Float]] =
  given Order[Point3d[Float]] = Order.by(_.z)
  genericContainer(
    (draw, meta) => drawAt(ffi, draw, meta.x, meta.y),
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    updateListOrdered
  )
end container

def linearContainer[IO[_] : {Monad, ForeignFunctionInterface}, Event] : LinearContainer[DesktopWidget[IO, Event], OuterPlace[IO, *], List, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[IO, Event],
    OuterPlace[IO, *],
    List,
    InfinityOr[Float],
    Float,
  ](
    container = container([A : Order, B] => v => f => orderedListProcessing(v)(f)),
    getBounds = OuterPlace.getBounds,
    setBounds = OuterPlace.setBounds,
    cut = _.minus(_)
  )
end linearContainer
