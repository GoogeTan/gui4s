package gui4s.desktop.kit
package widgets.decorator

import cats.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[IO[_] : Monad, Event]: UpdateDecorator[
  UpdateC[IO, Event],
  OuterPlace[IO, *],
  InnerPlace[DesktopPlacedWidget[IO, Event]],
  DownEvent
] = updateDecoratorWithRect[
  UpdateC[IO, Event], OuterPlace[IO, *], InnerPlace, Draw[IO], RecompositionReaction[IO], DownEvent
]
