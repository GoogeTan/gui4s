package gui4s.desktop.example.zio

import catnip.zio.resourceToScoped
import gui4s.core.geometry.*
import gui4s.desktop.kit.common.*
import gui4s.desktop.kit.zio.*
import gui4s.desktop.kit.zio.effects.*
import gui4s.desktop.kit.zio.widgets.*
import gui4s.desktop.kit.zio.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.glfw.OglGlfwWindow
import gui4s.glfw.WindowCreationSettings
import io.github.humbleui.skija.shaper.*
import zio.*
import zio.interop.catz.given
import cats.syntax.all.*

object ClickabeExample extends Gui4sZioApp:
  final case class PreInit(
                            shaper: Shaper,
                            globalTextCache: TextCache[Task],
                            mousePosition: Task[Point2d[Float]],
                            style : SkijaTextStyle
                          )

  override def preInit(backend: SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]): ZIO[Scope, Throwable, PreInit] =
    for
      shaper <- Draw.makeShaper
      cache: TextCache[Task] <- ScalacacheCache()
      typeface <- resourceToScoped(typeface.defaultTypeface)
      font <- resourceToScoped(font.makeFont(typeface, 24))
      paint <- resourceToScoped(paint.make.evalTap(paint.setColour(0xFF8484A4)))
    yield PreInit(shaper, cache, backend.mousePosition, SkijaTextStyle(font, paint))
  end preInit

  override val settings: WindowCreationSettings[Float] = WindowCreationSettings(
    title = "Gui4s image widget example",
    size = Rect(620f, 480f),
    visible = true,
    resizeable = true,
    debugContext = true
  )

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def main(preInit: PreInit): DesktopWidget[ApplicationRequest] =
    extension[Event] (value: DesktopWidget[Event])
      def onClick(event : Event) : DesktopWidget[Event] =
        clickCatcher(
          preInit.mousePosition,
          event
        )(value)
    end extension

    statefulWidget[Int, ApplicationRequest, Unit](
      name = "state",
      initialState = 0,
      eventHandler = (state, _, _) => (state + 1).pure[UpdateC[ApplicationRequest]],
      body = state =>
        text(
          preInit.shaper,
          preInit.globalTextCache
        )(
          "test text " + state.toString,
          preInit.style,
        ).onClick(())
    )
  end main
end ClickabeExample
