package gui4s.desktop.kit.widgets

import catnip.{Sortable, Zip}
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*
import cats.effect.*
import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.widget.library.LinearContainer
import gui4s.core.widget.library.linearContainer as genericLinearContainer
import gui4s.desktop.kit.effects.*


def linearContainerWidget[
  Event,
  Collection[_] : {Applicative, Traverse, Zip, Sortable}//TODO Refactor me. Лучше стоит принимать более конкретные операции. Это слишком сильное требование
](traverseOrdered: TraverseOrdered[UpdateC[IO, Event], Collection]) : LinearContainer[DesktopWidget[Event], PlacementEffect[IO, *], Collection, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[Event],
    PlacementEffect[IO, *],
    Collection,
    InfinityOr[Float],
    Float,
  ](
    container = containerWidget[Collection, Event](
      traverseOrdered,
      foldOrdered[Draw[IO], Collection],
    ),
    getBounds = PlacementEffect.getBounds,
    setBounds = PlacementEffect.setBounds,
    cut = _.minus(_)
  )
end linearContainerWidget

def linearListContainerWidget[Event] : LinearContainer[DesktopWidget[Event], PlacementEffect[IO, *], List, InfinityOr[Float], Float, Axis] =
  linearContainerWidget(traverseOrdered)
end linearListContainerWidget

def rowWidget[Event](
  children                    : List[DesktopWidget[Event]],
  horizontalPlacementStrategy : LinearContainerPlacementStrategy[List] = LinearContainerPlacementStrategy.Begin[List](0f),
  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Begin[Id](0f),
) : DesktopWidget[Event] =
  linearListContainerWidget(
    children,
    Axis.Horizontal,
    horizontalPlacementStrategy,
    verticalPlacementStrategy
  )
end rowWidget

def columnWidget[Event](
  children                    : List[DesktopWidget[Event]],
  verticalPlacementStrategy   : LinearContainerPlacementStrategy[List] = LinearContainerPlacementStrategy.Begin[List](0f),
  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Begin[Id](0f),
) : DesktopWidget[Event] =
  linearListContainerWidget(
    children,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end columnWidget
