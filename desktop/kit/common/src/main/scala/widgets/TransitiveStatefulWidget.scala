package gui4s.desktop.kit
package common.widgets

import common.effects.*
import common.effects.Update.given 

import cats.MonadThrow
import gui4s.desktop.widget.library.{TransitiveStatefulWidget, TransitiveStatefulWidgetFromStatefulWidget}

def transitiveStatefulWidget[IO[_] : MonadThrow]: TransitiveStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *]] =
  TransitiveStatefulWidgetFromStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *], [Value] =>> Value => RecompositionReaction[IO]](
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget