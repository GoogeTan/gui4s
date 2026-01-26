package gui4s.android.kit
package widgets

import catnip.Zip
import catnip.syntax.applicative.given
import catnip.syntax.list.TraverseOrdered
import catnip.syntax.list.traverseOne
import catnip.syntax.list.traverseOrdered
import catnip.syntax.zip.given
import cats.*
import cats.data.*
import cats.effect.kernel.Sync
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point3d
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.LinearContainer
import gui4s.core.widget.library.linearContainer as genericLinearContainer
import gui4s.core.kit.ContainerPlacementError
import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.Place.given
import gui4s.android.skia.canvas.drawAt
import gui4s.android.kit.effects.*
import gui4s.desktop.widget.library.container as genericContainer

def containerWidget[
  IO[_] : Sync,
  Collection[_] : Traverse,
  Event
](
  updateContainerOrdered : TraverseOrdered[UpdateC[IO, Event], Collection]
) : ContainerWidget[AndroidPlacedWidget[IO, Event], Collection, PlaceC[IO], Point3d[Float]] =
  given Order[Point3d[Float]] = Order.by(_.z)
  genericContainer(
    (draw, meta) => drawAt(meta.x, meta.y, draw),
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    updateContainerOrdered
  )
end containerWidget

def linearContainerWidget[
  IO[_] : Sync,
  Event,
  Collection[_] : {Applicative, Traverse, Zip}
](traverseOrdered: TraverseOrdered[UpdateC[IO, Event], Collection]) : LinearContainer[AndroidWidget[IO, Event], PlacementEffect[IO, *], Collection, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    AndroidPlacedWidget[IO, Event],
    PlacementEffect[IO, *],
    Collection,
    InfinityOr[Float],
    Float,
  ](
    container = containerWidget[IO, Collection, Event](traverseOrdered),
    getBounds = PlacementEffect.getBounds,
    setBounds = PlacementEffect.setBounds,
    cut = _.minus(_)
  )
end linearContainerWidget

def linearListContainerWidget[IO[_] : Sync, Event] : LinearContainer[AndroidWidget[IO, Event], PlacementEffect[IO, *], List, InfinityOr[Float], Float, Axis] =
  linearContainerWidget(traverseOrdered)
end linearListContainerWidget

def rowWidget[IO[_] : Sync, Event](
                              children                    : List[AndroidWidget[IO, Event]],
                              horizontalPlacementStrategy : LinearContainerPlacementStrategy[IO, List],
                              verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO],
) : AndroidWidget[IO, Event] =
  linearListContainerWidget(
    children,
    Axis.Horizontal,
    horizontalPlacementStrategy,
    verticalPlacementStrategy
  )
end rowWidget

def columnWidget[IO[_] : Sync, Event](
                                 children                    : List[AndroidWidget[IO, Event]],
                                 verticalPlacementStrategy   : LinearContainerPlacementStrategy[IO, List],
                                 horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO],
) : AndroidWidget[IO, Event] =
  linearListContainerWidget(
    children,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end columnWidget

def boxWidget[IO[_] : Sync, Event](
                                  child : AndroidWidget[IO, Event],
                                  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO],
                                  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO]
                              ) : AndroidWidget[IO, Event] =
  linearContainerWidget[IO, Event, Id](traverseOne)(
    child,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end boxWidget