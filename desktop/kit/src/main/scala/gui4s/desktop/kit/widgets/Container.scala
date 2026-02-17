package gui4s.desktop.kit
package widgets

import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*
import cats.effect.*
import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point3d
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.widget.library.ContainerWidget
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects.*
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.container as genericContainer
import gui4s.desktop.widget.library.widgetIsDrawable

def containerWidget[
  Collection[_] : Traverse,
  Event
](
  updateContainerOrdered : TraverseOrdered[UpdateC[IO, Event], Collection],
  drawOrdered : [A : Order] => Collection[A] => (A => Draw[IO]) => Draw[IO]
) : ContainerWidget[DesktopPlacedWidget[Event], Collection, PlaceC[IO], Point3d[Float]] =
  type WidgetAndItsPositionInContainer = (DesktopPlacedWidget[Event], Point3d[Float])
  genericContainer(
    [T] => (update, point) => Update.withCornerCoordinates(update, _ + point),
    Update.isEventHandled,
    children => updateFunction =>
      //Обновляются сначала виджеты, лежащие выше.
      given Order[WidgetAndItsPositionInContainer] = Order.reverse(Order.by(_._2.z))
      updateContainerOrdered(children)(updateFunction),
    children =>
      // А рисуются вначале виджеты, лежащие ниже.
      given Order[WidgetAndItsPositionInContainer] = Order.by(_._2.z)
      drawOrdered(children)((widget, meta) => drawAt(meta.x, meta.y, widgetIsDrawable(widget)))
  )
end containerWidget

def oneElementContainerWidget[Event] =
  containerWidget[Id, Event](
    traverseOne,
    foldOrdered[Draw[IO], Id]
  )
end oneElementContainerWidget

def listContainerWidget[Event] =
  containerWidget[List, Event](
    traverseOrdered,
    foldOrdered[Draw[IO], List]
  )
end listContainerWidget


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