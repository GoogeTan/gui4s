package gui4s.desktop.kit
package widgets

import catnip.syntax.applicative.given
import catnip.syntax.list.{TraverseOrdered, traverseOne, traverseOrdered}
import catnip.syntax.zip.given
import catnip.Zip
import cats.*
import cats.syntax.all.*
import cats.data.*
import cats.effect.kernel.Sync
import gui4s.core.geometry.{Axis, InfinityOr, Point2d, Point3d}
import gui4s.core.widget.library.*
import gui4s.core.widget.library.linearContainer as genericLinearContainer
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.container as genericContainer
import gui4s.core.widget.library.{ContainerWidget, LinearContainer, linearContainer as genericLinearContainer}

def container[
  IO[_] : Sync,
  Container[_] : Traverse,
  Event
](
  updateContainerOrdered : TraverseOrdered[UpdateC[IO, Event], Container]
) : ContainerWidget[DesktopPlacedWidget[IO, Event], Container, PlaceC[IO], Point3d[Float]] =
  given Order[Point3d[Float]] = Order.by(_.z)
  genericContainer(
    (draw, meta) => drawAt(meta.x, meta.y, draw),
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    updateContainerOrdered
  )
end container

def linearContainer[
  IO[_] : Sync as IOS,
  Event,
  Container[_] : {Applicative, Traverse as CT, Zip}
](traverseOrdered: TraverseOrdered[UpdateC[IO, Event], Container]) : LinearContainer[DesktopWidget[IO, Event], OuterPlace[IO, *], Container, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[IO, Event],
    OuterPlace[IO, *],
    Container,
    InfinityOr[Float],
    Float,
  ](
    container = container[IO, Container, Event](traverseOrdered),
    getBounds = OuterPlace.getBounds,
    setBounds = OuterPlace.setBounds,
    cut = _.minus(_)
  )
end linearContainer

def linearListContainer[IO[_] : Sync, Event] : LinearContainer[DesktopWidget[IO, Event], OuterPlace[IO, *], List, InfinityOr[Float], Float, Axis] =
  linearContainer(traverseOrdered)
end linearListContainer

def row[IO[_] : Sync, Event](
                              children                    : List[DesktopWidget[IO, Event]],
                              horizontalPlacementStrategy : LinearContainerPlacementStrategy[IO, List],
                              verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO],
) : DesktopWidget[IO, Event] =
  linearListContainer(
    children,
    Axis.Horizontal,
    horizontalPlacementStrategy,
    verticalPlacementStrategy
  )
end row

def column[IO[_] : Sync, Event](
                                 children                    : List[DesktopWidget[IO, Event]],
                                 verticalPlacementStrategy   : LinearContainerPlacementStrategy[IO, List],
                                 horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO],
) : DesktopWidget[IO, Event] =
  linearListContainer(
      children,
      Axis.Vertical,
      verticalPlacementStrategy,
      horizontalPlacementStrategy
    )
end column

def single[IO[_] : Sync, Event](
                                  child : DesktopWidget[IO, Event],
                                  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO],
                                  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO]
                              ) : DesktopWidget[IO, Event] =
  linearContainer[IO, Event, Id](traverseOne)(
    child,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end single
