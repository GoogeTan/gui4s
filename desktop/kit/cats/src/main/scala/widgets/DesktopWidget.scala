package gui4s.desktop.kit.cats
package widgets

import effects.*

import cats.effect.IO
import gui4s.core.kit.effects.RecompositionReaction
import gui4s.desktop.kit.common.widgets.{DesktopPlacedWidget, DesktopWidget}
import gui4s.desktop.widget.library.Widget 

type DesktopPlacedWidget[Event] = gui4s.desktop.kit.common.widgets.DesktopPlacedWidget[IO, Event]
type DesktopWidget[Event] = gui4s.desktop.kit.common.widgets.DesktopWidget[IO, Event]
