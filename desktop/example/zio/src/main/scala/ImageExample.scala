
package gui4s.desktop.example.zio

import catnip.ForeignFunctionInterface
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.decktop.widget.library.decorator.Paddings
import gui4s.desktop.kit.zio.*
import gui4s.desktop.kit.zio.effects.*
import gui4s.desktop.kit.zio.widgets.*
import gui4s.desktop.kit.zio.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.kit.generic.SkijaBackend
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import scalacache.caffeine.CaffeineCache
import zio.*
import zio.http.*
import zio.interop.catz.*
import catnip.zio.*
import scala.reflect.Typeable

object ImageExample extends ZIOAppDefault:
  given ffi: ForeignFunctionInterface[Task] = new ZioForeignFunctionInterface()

  final case class PreInit(
                            shaper: Shaper,
                            globalTextCache: TextCache[Task],
                            raiseEvent: DownEvent => Task[Unit]
                          )

  def preInit(backend: SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]): ZIO[Scope, Throwable, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache: TextCache[Task] <- CaffeineCache[Task, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]].map(ScalacacheCache(_)).orDie
    yield PreInit(shaper, cache, backend.raiseEvent)
  end preInit

  override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
    desktopApp[
      PreInit
    ](
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
    ).provide(Client.default)
  end run

  def main(preInit: PreInit): DesktopWidget[ApplicationRequest] =
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
    given Typeable[Task[Unit]] = a => a match
      case b: Task[t] => Some(b.unit.asInstanceOf[Task[Unit] & a.type])
      case _ => None


    def downloadImage(uri: String): ZIO[Client, Throwable, Image] =
      for
        client <- ZIO.service[Client]
        url <- ZIO.fromEither(URL.decode(uri))
        res <- client.url(url)
        body <- res.body.asArray
      yield Image.makeDeferredFromEncodedBytes(response)
    end downloadImage

    initWidget(
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
