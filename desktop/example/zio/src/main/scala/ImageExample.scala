package gui4s.desktop.example.zio

import catnip.ForeignFunctionInterface
import catnip.zio.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.SkijaBackend
import gui4s.desktop.kit.zio.*
import gui4s.desktop.kit.zio.effects.*
import gui4s.desktop.kit.zio.widgets.*
import gui4s.desktop.kit.zio.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.widget.library.decorator.Paddings
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import zio.*
import zio.http.*
import zio.interop.catz.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object ImageExample extends Gui4sZioApp:
  given ffi: ForeignFunctionInterface[Task] = new ZioForeignFunctionInterface()

  type PreInit = ImagePreInit

  final case class ImagePreInit(
                                  shaper: Shaper,
                                  globalTextCache: TextCache[Task],
                                  raiseEvent: DownEvent => Task[Unit],
                                  supervisor : cats.effect.std.Supervisor[Task]
                                )

  override def preInit(backend: SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]): ZIO[Scope, Throwable, PreInit] =
    for
      shaper <- Draw.makeShaper
      cache: TextCache[Task] <- ScalacacheCache()
      supervisor <- CatsSupervisor
    yield ImagePreInit(shaper, cache, backend.raiseEvent, supervisor)
  end preInit

  override val settings: WindowCreationSettings[Float] = WindowCreationSettings(
    title = "Gui4s image widget example",
    size = Rect(620f, 480f),
    visible = true,
    resizeable = true,
    debugContext = true
  )

  def main(preInit: PreInit): DesktopWidget[ApplicationRequest] =
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def downloadImage(uri: String): ZIO[Any, Throwable, Image] =
      for
        parsedUrl <- ZIO.fromEither(URL.decode(uri)).mapError(new IllegalArgumentException("Invalid URL", _))
        request = Request.get(parsedUrl)
        response <- Client.batched(request).provide(Scope.default ++ Client.default)
        bytes <-
          if response.status.isSuccess then
            response.body.asArray
          else
            ZIO.fail(new RuntimeException(s"Failed to download image: HTTP status ${response.status.code}"))
          end if
      yield Image.makeDeferredFromEncodedBytes(bytes)
    end downloadImage

    initWidget(
      supervisor = preInit.supervisor,
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
