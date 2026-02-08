package gui4s.desktop.kit.widgets

import catnip.Zip
import catnip.syntax.applicative.given
import catnip.syntax.list.TraverseOrdered
import catnip.syntax.list.traverseOne
import catnip.syntax.list.traverseOrdered
import catnip.syntax.zip.given
import cats._
import cats.data._
import cats.effect.kernel.Sync

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point3d
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.LinearContainer
import gui4s.core.widget.library.{linearContainer => genericLinearContainer}

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.{container => genericContainer}


def linearContainerWidget[
  IO[_] : Sync,
  Event,
  Collection[_] : {Applicative, Traverse, Zip}
](traverseOrdered: TraverseOrdered[UpdateC[IO, Event], Collection]) : LinearContainer[DesktopWidget[IO, Event], PlacementEffect[IO, *], Collection, InfinityOr[Float], Float, Axis] =
  genericLinearContainer[
    DesktopPlacedWidget[IO, Event],
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

def linearListContainerWidget[IO[_] : Sync, Event] : LinearContainer[DesktopWidget[IO, Event], PlacementEffect[IO, *], List, InfinityOr[Float], Float, Axis] =
  linearContainerWidget(traverseOrdered)
end linearListContainerWidget

def rowWidget[IO[_], Event](using Sync[IO])(
  children                    : List[DesktopWidget[IO, Event]],
  horizontalPlacementStrategy : LinearContainerPlacementStrategy[IO, List] = LinearContainerPlacementStrategy.Begin[IO, List](0f),
  verticalPlacementStrategy   : OneElementLinearContainerPlacementStrategy[IO] = LinearContainerPlacementStrategy.Begin[IO, Id](0f),
) : DesktopWidget[IO, Event] =
  linearListContainerWidget(
    children,
    Axis.Horizontal,
    horizontalPlacementStrategy,
    verticalPlacementStrategy
  )
end rowWidget

def columnWidget[IO[_], Event](using Sync[IO])(
  children                    : List[DesktopWidget[IO, Event]],
  verticalPlacementStrategy   : LinearContainerPlacementStrategy[IO, List] = LinearContainerPlacementStrategy.Begin[IO, List](0f),
  horizontalPlacementStrategy : OneElementLinearContainerPlacementStrategy[IO] = LinearContainerPlacementStrategy.Begin[IO, Id](0f),
) : DesktopWidget[IO, Event] =
  linearListContainerWidget(
    children,
    Axis.Vertical,
    verticalPlacementStrategy,
    horizontalPlacementStrategy
  )
end columnWidget
