package gui4s.desktop.kit.zio
package widgets

import effects.*
import zio.*

import gui4s.core.kit.effects.RecompositionReaction
import gui4s.desktop.widget.library.Widget 

type DesktopPlacedWidget[Event] = gui4s.desktop.kit.widgets.DesktopPlacedWidget[Task, Event] 
type DesktopWidget[Event] =  gui4s.desktop.kit.widgets.DesktopWidget[Task, Event] 
