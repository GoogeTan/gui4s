package gui4s.android.kit.widgets

import gui4s.core.geometry.{Axis, InfinityOr, Point2d, Point3d}
import gui4s.core.widget.library.{ContainerWidget, LinearContainer, linearContainer as genericLinearContainer}
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.Place.given
import gui4s.android.skia.canvas.drawAt
import gui4s.desktop.widget.library.container as genericContainer

def container[
  IO[_] : Sync,
  Container[_] : Traverse,
  Event
](
  updateContainerOrdered : TraverseOrdered[UpdateC[IO, Event], Container]
) : ContainerWidget[AndroidPlacedWidget[IO, Event], Container, PlaceC[IO], Point3d[Float]] =
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
](traverseOrdered: TraverseOrdered[UpdateC[IO, Event], Container]) : LinearContainer[AndroidWidget[IO, Event], OuterPlace[IO, *], Container, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    AndroidPlacedWidget[IO, Event],
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

def linearListContainer[IO[_] : Sync, Event] : LinearContainer[AndroidWidget[IO, Event], OuterPlace[IO, *], List, InfinityOr[Float], Float, Axis] =
  linearContainer(traverseOrdered)
end linearListContainer

def row[IO[_] : Sync, Event](
                              children                    : List[AndroidWidget[IO, Event]],
                              horizontalPlacementStrategy : LinearContainerPlacementStrategy[IO, List],
                              verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO],
) : AndroidWidget[IO, Event] =
  linearListContainer(
    children,
    Axis.Horizontal,
    horizontalPlacementStrategy,
    verticalPlacementStrategy
  )
end row

def column[IO[_] : Sync, Event](
                                 children                    : List[AndroidWidget[IO, Event]],
                                 verticalPlacementStrategy   : LinearContainerPlacementStrategy[IO, List],
                                 horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO],
) : AndroidWidget[IO, Event] =
  linearListContainer(
      children,
      Axis.Vertical,
      verticalPlacementStrategy,
      horizontalPlacementStrategy
    )
end column

def single[IO[_] : Sync, Event](
                                  child : AndroidWidget[IO, Event],
                                  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO],
                                  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO]
                              ) : AndroidWidget[IO, Event] =
  linearContainer[IO, Event, Id](traverseOne)(
    child,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end single
