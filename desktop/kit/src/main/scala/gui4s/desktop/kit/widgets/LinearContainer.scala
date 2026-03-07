package gui4s.desktop.kit.widgets

import catnip.{Sortable, Zip}
import catnip.syntax.all.given
import cats.*
import cats.data.*
import cats.effect.*
import gui4s.core.geometry.{Axis, InfinityOr}
import gui4s.core.widget.library.{LinearContainer, linearContainer as genericLinearContainer}
import gui4s.desktop.widget.library.widgetAsFree
import gui4s.desktop.kit.effects.*


def linearContainerWidget[
  Event,
  Collection[_] : {Traverse, Zip, Sortable}//TODO Refactor me. Лучше стоит принимать более конкретные операции. Это слишком сильное требование
] : LinearContainer[DesktopWidget[Event], PlacementEffect, Collection, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[Event],
    PlacementEffect,
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
  children                    : List[DesktopWidget[Event]],
  horizontalPlacementStrategy : LinearContainerPlacementStrategy[List] = LinearContainerPlacementStrategy.Begin[List](0f),
  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy = LinearContainerPlacementStrategy.Begin[Id](0f),
) : DesktopWidget[Event] =
  linearContainerWidget[Event, List](
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
  linearContainerWidget[Event, List](
    children,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end columnWidget
