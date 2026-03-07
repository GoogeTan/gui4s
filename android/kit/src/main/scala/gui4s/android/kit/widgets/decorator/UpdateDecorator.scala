package gui4s.android.kit.widgets.decorator

import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.desktop.widget.library.decorator.*

import cats.effect.IO
import cats.Monad
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[Event],
  PlacementEffect,
  Situated[AndroidPlacedWidget[Event]],
  DownEvent
] = updateDecoratorWithRect[
  UpdateC[Event], PlacementEffect, Situated, Draw, RecompositionReaction, DownEvent
]
