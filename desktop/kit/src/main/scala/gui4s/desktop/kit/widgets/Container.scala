package gui4s.desktop.kit
package widgets

import catnip.syntax.applicative.given
import catnip.syntax.list.traverseListOrdered
import catnip.syntax.zip.given
import cats.*
import cats.data.*
import cats.effect.kernel.Sync
import gui4s.core.geometry.{Axis, InfinityOr, Point3d}
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.{ContainerWidget, LinearContainer, container as genericContainer, linearContainer as genericLinearContainer}

def container[
  IO[_] : Sync,
  Container[_] : Traverse,
  Event
](
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[IO, Event, Container[B]]) => Update[IO, Event, Container[B]]
) : ContainerWidget[DesktopPlacedWidget[IO, Event], Container, PlaceC[IO], Point3d[Float]] =
  given Order[Point3d[Float]] = Order.by(_.z)
  genericContainer(
    (draw, meta) => drawAt(meta.x, meta.y, draw),
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    updateListOrdered
  )
end container

def zIndexContainer[IO[_] : Sync, Event] = ()

def linearContainer[IO[_] : Sync, Event] : LinearContainer[DesktopWidget[IO, Event], OuterPlace[IO, *], List, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[IO, Event],
    OuterPlace[IO, *],
    List,
    InfinityOr[Float],
    Float,
  ](
    container = container([A : Order, B] => v => f => traverseListOrdered(v)(f)),
    getBounds = OuterPlace.getBounds,
    setBounds = OuterPlace.setBounds,
    cut = _.minus(_)
  )
end linearContainer
