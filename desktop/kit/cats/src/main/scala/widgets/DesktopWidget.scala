package gui4s.desktop.kit.cats
package widgets

import effects.*
import cats.effect.IO

import gui4s.core.kit.effects.RecompositionReaction
import gui4s.desktop.widget.library.Widget 

type DesktopPlacedWidget[Event] = gui4s.desktop.kit.widgets.DesktopPlacedWidget[IO, Event] 
type DesktopWidget[Event] =  gui4s.desktop.kit.widgets.DesktopWidget[IO, Event] 
