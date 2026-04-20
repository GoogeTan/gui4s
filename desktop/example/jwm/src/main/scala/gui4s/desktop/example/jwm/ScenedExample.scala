package gui4s.desktop.example.jwm

import cats.effect.IO
import cats.effect.std.Queue
import gui4s.desktop.example.shared.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.{scrollWidget as _, *}
import gui4s.desktop.windowing.jwm.*
import io.github.humbleui.jwm.Window

object ScenedExample extends UIApp:
  override def main(
    window : Window,
    eventBus: Queue[IO, DownEvent]
  ): Init[DesktopWidget[Nothing]] =
    scenedExample(using clickCatcher, scrollEventSource, eventBus)
  end main
end ScenedExample
