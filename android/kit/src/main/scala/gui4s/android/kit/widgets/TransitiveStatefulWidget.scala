package gui4s.android.kit.widgets

import cats.effect.IO
import gui4s.core.widget.library.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given

def transitiveStatefulWidget: TransitiveStatefulWidget[
  AndroidWidget[*],
  Update[*, *],
  * => RecompositionReaction,
  MergeStates[PlaceC, *]
] =
  TransitiveStatefulWidgetFromStatefulWidget(
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget