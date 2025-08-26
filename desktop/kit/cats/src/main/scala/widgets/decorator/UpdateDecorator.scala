package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.Update.given
import effects.OuterPlace.given
import widgets.*
import gui4s.decktop.widget.library.decorator.*

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[Event],
  OuterPlace,
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] = updateDecoratorWithRect[
  UpdateC[Event], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent
]
