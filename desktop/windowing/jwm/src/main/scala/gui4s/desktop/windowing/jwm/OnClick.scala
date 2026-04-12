package gui4s.desktop.windowing.jwm

import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.decorator.ClickCatcher

def clickCatcher : ClickCatcher =
  gui4s.desktop.kit.widgets.decorator.clickCatcher(jwmClickEventSource)
end clickCatcher

extension[Event](self: DesktopWidget[Event])
  def onClick(eventOnClick: Event) : DesktopWidget[Event] =
    clickCatcher(eventOnClick)(self)
  end onClick
end extension