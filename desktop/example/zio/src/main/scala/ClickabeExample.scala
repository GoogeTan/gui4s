
package gui4s.desktop.example.zio

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.desktop.kit.zio.*
import gui4s.desktop.kit.zio.effects.{*, given}
import gui4s.desktop.kit.zio.effects.Update.given
import gui4s.desktop.kit.zio.effects.Place.given
import gui4s.desktop.kit.zio.widgets.*
import gui4s.desktop.kit.zio.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.kit.generic.SkijaBackend
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import scalacache.caffeine.CaffeineCache
import catnip.zio.ZioForeignFunctionInterface
import cats.syntax.all.*
import zio.*
import zio.interop.catz.*

object ClickabeExample extends ZIOAppDefault:
  given ffi: ForeignFunctionInterface[Task] = new ZioForeignFunctionInterface()

  final case class PreInit(shaper: Shaper, globalTextCache: TextCache[Task], mousePosition: Task[Point2d[Float]])

  def preInit(backend: SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]): ZIO[Scope, Throwable, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache: TextCache[Task] <- CaffeineCache[Task, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]].map(ScalacacheCache(_)).orDie
    yield PreInit(shaper, cache, backend.mousePosition)
  end preInit

  override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
    desktopApp(
      preInit = preInit,
      main = main,
      settings = WindowCreationSettings(
        title = "Gui4s image widget example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      ),
      ffi = ffi,
    )
  end run


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
