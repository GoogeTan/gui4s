package gui4s.desktop.kit.cats
package widgets

import effects.*
import effects.Update.given

import gui4s.decktop.widget.library.{TransitiveStatefulWidget, TransitiveStatefulWidgetFromStatefulWidget}

def transitiveStatefulWidget: TransitiveStatefulWidget[DesktopWidget, Update] =
  TransitiveStatefulWidgetFromStatefulWidget[DesktopWidget, Update, [Value] =>> Value => RecompositionReaction](
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
