package gui4s.android.kit.widgets

import gui4s.core.widget.library.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given

def transitiveStatefulWidget[IO[_] : MonadThrow]: TransitiveStatefulWidget[
  AndroidWidget[IO, *],
  Update[IO, *, *],
  [Value] =>> Value => RecompositionReaction[IO],
  [State] =>> MergeStates[PlaceC[IO], State]
] =
  TransitiveStatefulWidgetFromStatefulWidget(
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget