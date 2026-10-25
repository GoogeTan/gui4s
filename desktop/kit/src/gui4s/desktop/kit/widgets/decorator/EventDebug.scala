package gui4s.desktop.kit.widgets.decorator

import gui4s.desktop.kit.effects.Update
import gui4s.desktop.kit.widgets.DesktopWidget

def eventDebug[Event](
  original : DesktopWidget[Event],
  before : Update[Event, Unit] = Update.unit,
  after : Update[Event, Unit] = Update.unit,
) : DesktopWidget[Event] =
  gui4s.desktop.widget.library.decorator.eventDebug(
    original,
    before,
    after
  )
end eventDebug

extension[Event](value : DesktopWidget[Event])
  def debug(
    before : Update[Event, Unit] = Update.unit,
    after : Update[Event, Unit] = Update.unit,
  ): DesktopWidget[Event] =
    eventDebug(value, before, after)
  end debug
end extension
