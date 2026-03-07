package gui4s.android.kit.widgets

import catnip.{Sortable, Zip}
import catnip.syntax.all.given
import cats.*
import cats.data.*
import cats.effect.*
import gui4s.core.geometry.{Axis, InfinityOr}
import gui4s.core.widget.library.{LinearContainer, linearContainer as genericLinearContainer}
import gui4s.desktop.widget.library.widgetAsFree
import gui4s.android.kit.effects.*

import cats.effect.IO
import catnip.{Sortable, Zip}
import catnip.syntax.all.given
import cats.*
import cats.data.*
import cats.effect.*
import gui4s.core.geometry.{Axis, InfinityOr}
import gui4s.core.widget.library.{LinearContainer, linearContainer as genericLinearContainer}
import gui4s.desktop.widget.library.widgetAsFree
import gui4s.android.kit.effects.*

def linearContainerWidget[
  Event,
  Collection[_] : {Traverse, Zip, Sortable}
] : LinearContainer[AndroidWidget[Event], PlacementEffect[*], Collection, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    AndroidPlacedWidget[Event],
    PlacementEffect[*],
    Collection,
    InfinityOr[Float],
    Float,
  ](
    container = containerWidget[Collection, Event],
    getBounds = PlacementEffect.getBounds,
    setBounds = PlacementEffect.setBounds,
    cut = _.minus(_),
    widgetAsFree = widgetAsFree
  )
end linearContainerWidget

def rowWidget[Event](
  children : List[AndroidWidget[Event]],
) : AndroidWidget[Event] =
  rowWidget(
    children,
    LinearContainerPlacementStrategy.Begin[List](0f),
    LinearContainerPlacementStrategy.Begin[Id](0f)
  )
end rowWidget

def rowWidget[Event](
  children                    : List[AndroidWidget[Event]],
  horizontalPlacementStrategy : LinearContainerPlacementStrategy[List],
  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy,
) : AndroidWidget[Event] =
  linearContainerWidget[Event, List](
    children,
    Axis.Horizontal,
    horizontalPlacementStrategy,
    verticalPlacementStrategy
  )
end rowWidget

def columnWidget[Event](
  children : List[AndroidWidget[Event]],
) : AndroidWidget[Event] =
  columnWidget(
    children,
    LinearContainerPlacementStrategy.Begin[List](0f),
    LinearContainerPlacementStrategy.Begin[Id](0f)
  )
end columnWidget

def columnWidget[Event](
  children                    : List[AndroidWidget[Event]],
  verticalPlacementStrategy   : LinearContainerPlacementStrategy[List],
  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy,
) : AndroidWidget[Event] =
  linearContainerWidget[Event, List](
    children,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end columnWidget
