package gui4s.desktop.kit.cats
package widgets

import cats.effect.IO 

type DesktopPlacedWidget[Event] = gui4s.desktop.kit.common.widgets.DesktopPlacedWidget[IO, Event]
type DesktopWidget[Event] = gui4s.desktop.kit.common.widgets.DesktopWidget[IO, Event]
