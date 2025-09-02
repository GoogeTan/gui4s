package gui4s.desktop.example.zio

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import catnip.zio.ZioForeignFunctionInterface
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.SkijaBackend
import gui4s.desktop.kit.zio.*
import gui4s.desktop.kit.zio.effects.{*, given}
import gui4s.desktop.kit.zio.widgets.*
import gui4s.desktop.kit.zio.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import zio.*
import zio.interop.catz.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object ClickabeExample extends Gui4sZioApp:
  given ffi: ForeignFunctionInterface[Task] = new ZioForeignFunctionInterface()

  final case class PreInit(shaper: Shaper, globalTextCache: TextCache[Task], mousePosition: Task[Point2d[Float]])

  override def preInit(backend: SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]): ZIO[Scope, Throwable, PreInit] =
    for
      shaper <- Draw.makeShaper
      cache: TextCache[Task] <- ScalacacheCache()
    yield PreInit(shaper, cache, backend.mousePosition)
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
    extension (value: DesktopWidget[Unit])
      def onClick(event: Unit): DesktopWidget[Unit] =
        clickCatcher(preInit.mousePosition, event)(value)
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
          SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
        ).onClick(())
    )
  end main
end ClickabeExample
