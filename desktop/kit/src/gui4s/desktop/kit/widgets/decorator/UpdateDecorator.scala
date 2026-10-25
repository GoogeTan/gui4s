package gui4s.desktop.kit
package widgets.decorator

import cats.*
import cats.effect.*

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[Event],
  PlacementEffect,
  Situated[DesktopPlacedWidget[Event]]
] = updateDecoratorWithRect[
  UpdateC[Event], PlacementEffect, Situated, Draw, RecompositionReaction
]
