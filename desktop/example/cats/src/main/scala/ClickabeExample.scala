package gui4s.desktop.example.cats

import catnip.ForeignFunctionInterface
import catnip.effect.SyncForeignFunctionInterface
import catnip.syntax.all.given
import cats.*
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.*
import gui4s.desktop.kit.cats.*
import gui4s.desktop.kit.cats.effects.{ApplicationRequest, DownEvent, UpdateC}
import gui4s.desktop.kit.cats.widgets.*
import gui4s.desktop.kit.cats.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper

object ClickabeExample extends IOApp:
  given ffi : ForeignFunctionInterface[IO] = SyncForeignFunctionInterface[IO]

  final case class PreInit(shaper : Shaper, globalTextCache : TextCache[IO], mousePosition : IO[Point2d[Float]])

  def preInit(backend : gui4s.desktop.kit.SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) : Resource[IO, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- ScalacacheCache()
    yield PreInit(shaper, cache, backend.mousePosition)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    desktopApp(
      preInit = preInit,
      main = main,
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      settings = WindowCreationSettings(
        title = "Gui4s image widget example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      ),
    )
  end run


  def main(preInit : PreInit) : DesktopWidget[ApplicationRequest] =
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
