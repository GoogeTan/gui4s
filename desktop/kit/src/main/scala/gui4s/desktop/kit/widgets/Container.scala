package gui4s.desktop.kit
package widgets

import catnip.syntax.applicative.given
import catnip.syntax.list.TraverseOrdered
import catnip.syntax.list.traverseOne
import catnip.syntax.zip.given
import cats._
import cats.data._
import cats.effect.*

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point3d
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.widget.library.ContainerWidget

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.{container => genericContainer}

def containerWidget[
  Collection[_] : Traverse,
  Event
](
  updateContainerOrdered : TraverseOrdered[UpdateC[IO, Event], Collection]
) : ContainerWidget[DesktopPlacedWidget[Event], Collection, PlaceC[IO], Point3d[Float]] =
  given Order[Point3d[Float]] = Order.by(_.z)
  genericContainer(
    (draw, meta) => drawAt(meta.x, meta.y, draw),
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    updateContainerOrdered
  )
end containerWidget


def boxWidget[Event](
  child : DesktopWidget[Event],
  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English),
  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English)
) : DesktopWidget[Event] =
  linearContainerWidget[Event, Id](traverseOne)(
    child,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end boxWidget