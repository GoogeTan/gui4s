package gui4s.desktop.kit
package common.widgets.decorator

import cats.*
import gui4s.desktop.kit.common.widgets.DesktopPlacedWidget
import gui4s.desktop.kit.common.effects.*
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[IO[_] : Monad, Event]: UpdateDecorator[
  UpdateC[IO, Event],
  OuterPlace[IO, *],
  InnerPlace[DesktopPlacedWidget[IO, Event]],
  DownEvent
] = updateDecoratorWithRect[
  UpdateC[IO, Event], OuterPlace[IO, *], InnerPlace, Draw[IO], RecompositionReaction[IO], DownEvent
]
