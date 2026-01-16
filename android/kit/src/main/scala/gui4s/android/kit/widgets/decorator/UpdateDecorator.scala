package gui4s.android.kit.widgets.decorator

import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[IO[_] : Monad, Event]: UpdateDecorator[
  UpdateC[IO, Event],
  OuterPlace[IO, *],
  InnerPlace[AndroidPlacedWidget[IO, Event]],
  DownEvent
] = updateDecoratorWithRect[
  UpdateC[IO, Event], OuterPlace[IO, *], InnerPlace, Draw[IO], RecompositionReaction[IO], DownEvent
]
