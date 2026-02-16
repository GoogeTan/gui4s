package gui4s.desktop.kit
package widgets

import cats.effect.IO

import gui4s.core.widget.library.TransitiveStatefulWidget
import gui4s.core.widget.library.TransitiveStatefulWidgetFromStatefulWidget
import gui4s.core.widget.library._

import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.kit.effects._

def transitiveStatefulWidget: TransitiveStatefulWidget[
  DesktopWidget,
  Update[IO, *, *],
  [Value] =>> Value => RecompositionReaction[IO],
  [State] =>> MergeStates[PlaceC[IO], State]
] =
  TransitiveStatefulWidgetFromStatefulWidget(
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget