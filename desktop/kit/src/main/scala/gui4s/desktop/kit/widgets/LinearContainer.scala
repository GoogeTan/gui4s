package gui4s.desktop.kit.widgets

import catnip.Sortable
import catnip.Zip
import catnip.syntax.all.given
import cats._
import cats.data._
import cats.effect._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.widget.library.LinearContainer
import gui4s.core.widget.library.{linearContainer => genericLinearContainer}

import gui4s.desktop.kit.effects._
import gui4s.desktop.widget.library.widgetAsFree


def linearContainerWidget[
  Event,
  Collection[_] : {Traverse, Zip, Sortable}//TODO Refactor me. Лучше стоит принимать более конкретные операции. Это слишком сильное требование
] : LinearContainer[DesktopWidget[Event], PlacementEffect, Collection, Float, InfinityOr[Float], Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[Event],
    PlacementEffect,
    Collection,
    Float,
    InfinityOr[Float],
  ](
    container = containerWidget[Collection, Event],
    getBounds = PlacementEffect.getBounds,
    withBounds = (newBounds, effect) => PlacementEffect.withBounds(effect, _ => newBounds),
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
