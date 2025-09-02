package gui4s.desktop.example.cats

import catnip.ForeignFunctionInterface
import catnip.effect.SyncForeignFunctionInterface
import cats.*
import cats.effect.std.{Dispatcher, Supervisor}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.desktop.kit.*
import gui4s.desktop.kit.cats.*
import gui4s.desktop.kit.cats.effects.{ApplicationRequest, DownEvent, Shapes}
import gui4s.desktop.kit.cats.widgets.*
import gui4s.desktop.kit.cats.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.widget.library.decorator.Paddings
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import org.http4s.Uri
import org.http4s.ember.client.*
import scalacache.caffeine.CaffeineCache

import scala.reflect.Typeable

object ImageExample extends IOApp:
  given ffi: ForeignFunctionInterface[IO] = SyncForeignFunctionInterface[IO]

  final case class PreInit(
                            dispatcher: Dispatcher[IO],
                            globalSupervisor: Supervisor[IO],
                            shaper: Shaper,
                            globalTextCache: TextCache[IO],
                            raiseEvent : DownEvent => IO[Unit]
                          )

  def preInit(backend: gui4s.desktop.kit.SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]): Resource[IO, PreInit] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      supervisor <- Supervisor[IO]
      shaper <- backend.skija.createShaper
      cache: TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(ScalacacheCache(_))
    yield PreInit(dispatcher, supervisor, shaper, cache, backend.raiseEvent)
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
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
    given Typeable[IO[Unit]] = a => a match
      case b: IO[t] => Some(b.as(()).asInstanceOf[IO[Unit] & a.type])
      case _ => None

    def downloadImage(uri: String): IO[Image] =
      EmberClientBuilder
        .default[IO]
        .build
        .use(
          client =>
            IO.fromEither(
              Uri.fromString(uri)
            ).flatMap(
              client.expect[Array[Byte]]
            ).map(Image.makeDeferredFromEncodedBytes)
        )
    end downloadImage

    initWidget(
      supervisor = preInit.globalSupervisor,
      raiseExternalEvent = preInit.raiseEvent
    )(
      name = "image",
      imageSource = downloadImage("https://i.pinimg.com/1200x/1b/6e/8c/1b6e8c66f6d302c0c0156104a52a32be.jpg"),
      imageWidget = image,
      placeholder = text(preInit.shaper, preInit.globalTextCache)("Wait.", SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))),
    ).clip(
      Shapes.round
    ).gapPadding(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end ImageExample
