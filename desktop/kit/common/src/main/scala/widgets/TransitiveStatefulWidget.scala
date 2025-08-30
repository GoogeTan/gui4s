package gui4s.desktop.kit
package widgets

import effects.*
import effects.Update.given

import gui4s.decktop.widget.library.{TransitiveStatefulWidget, TransitiveStatefulWidgetFromStatefulWidget}

def transitiveStatefulWidget[IO[_]]: TransitiveStatefulWidget[DesktopWidget[IO, *], Update] =
  TransitiveStatefulWidgetFromStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *], [Value] =>> Value => RecompositionReaction](
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
