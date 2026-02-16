package gui4s.desktop.kit
package widgets.decorator

import cats._
import cats.effect._

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library.decorator._

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[IO, Event],
  PlacementEffect[IO, *],
  Situated[DesktopPlacedWidget[Event]],
  DownEvent
] = updateDecoratorWithRect[
  UpdateC[IO, Event], PlacementEffect[IO, *], Situated, Draw[IO], RecompositionReaction[IO], DownEvent
]
