package gui4s.desktop.kit.zio
package widgets

import effects.*

import gui4s.decktop.widget.library.Widget

type DesktopPlacedWidget[Event] = Widget[UpdateC[Event], Place, Draw, RecompositionReaction, DownEvent]
type DesktopWidget[Event] = Place[DesktopPlacedWidget[Event]]
