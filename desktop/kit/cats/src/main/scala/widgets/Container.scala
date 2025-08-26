package gui4s.desktop.kit.cats
package widgets

import effects.*
import effects.Update.given
import effects.OuterPlace.given
import effects.Place.given

import catnip.effect.SyncForeignFunctionInterface
import catnip.syntax.list.orderedListProcessing
import catnip.syntax.zip.given
import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, InfinityOr, Point3d}
import gui4s.decktop.widget.library.{ContainerWidget, LinearContainer, container as genericContainer, linearContainer as genericLinearContainer}
import gui4s.desktop.skija.drawAt

def container[Container[_] : Traverse, Event](
                                                updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[Event, Container[B]]) => Update[Event, Container[B]]
                                              ) : ContainerWidget[DesktopPlacedWidget[Event], Container, Place, Point3d[Float]] =
  given Order[Point3d[Float]] = Order.by(_.z)
  genericContainer(
    (draw, meta) => drawAt(SyncForeignFunctionInterface[IO](), draw, meta.x, meta.y),
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    updateListOrdered
  )
end container

def linearContainer[Event] : LinearContainer[DesktopWidget[Event], OuterPlace, List, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[Event],
    OuterPlace,
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
