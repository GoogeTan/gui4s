package gui4s.desktop.kit
package widgets

import cats.MonadThrow
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Update.given
import gui4s.core.widget.library.{TransitiveStatefulWidget, TransitiveStatefulWidgetFromStatefulWidget}
import gui4s.core.widget.library.*

def transitiveStatefulWidget[IO[_] : MonadThrow]: TransitiveStatefulWidget[
  DesktopWidget[IO, *],
  Update[IO, *, *],
  [Value] =>> Value => RecompositionReaction[IO],
  [State] =>> MergeStates[PlaceC[IO], State]
] =
  TransitiveStatefulWidgetFromStatefulWidget(
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget