package gui4s.desktop.kit
package widgets

import gui4s.core.widget.library.TransitiveStatefulWidget
import gui4s.core.widget.library.TransitiveStatefulWidgetFromStatefulWidget
import gui4s.core.widget.library._

import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.kit.effects._

def transitiveStatefulWidget: TransitiveStatefulWidget[
  DesktopWidget,
  Update,
  * => RecompositionReaction,
  MergeStates[Place, *]
] =
  TransitiveStatefulWidgetFromStatefulWidget(
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget