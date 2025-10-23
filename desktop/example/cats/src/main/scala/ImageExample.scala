package gui4s.desktop.example.cats

import cats.data.EitherT
import cats.effect.*
import cats.effect.std.*
import gui4s.desktop.kit.common.*
import gui4s.desktop.widget.library.decorator.*
import gui4s.desktop.kit.common.widgets.decorator.*
import gui4s.desktop.kit.common.widgets.*
import gui4s.desktop.kit.common.effects.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.*
import gui4s.desktop.skija.*
import glfw4s.core.*
import glfw4s.jvm.types.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.Uri

import scala.reflect.Typeable

object ImageExample extends CatsApp:
  final case class PreInit(
                            dispatcher: Dispatcher[CatsIO],
                            globalSupervisor: Supervisor[CatsIO],
                            shaper: Shaper,
                            globalTextCache: TextCache[CatsIO],
                            raiseEvent : DownEvent => CatsIO[Unit]
                          )

  val settings = WindowCreationSettings(
    title = "Gui4s image example",
    width = 620,
    height = 480,
  )

  def preInit(backend: SkijaBackend[CatsIO, Resource[CatsIO, *], IO, GLFWmonitor, GLFWwindow, DownEvent]): Resource[CatsIO, PreInit] =
    for
      dispatcher <- Dispatcher.sequential[CatsIO]
      supervisor <- Supervisor[CatsIO]
      shaper <- createShaper[CatsIO]
      cache: TextCache[CatsIO] <- ScalacacheCache()
    yield PreInit(dispatcher, supervisor, shaper, cache, backend.raiseEvent)
  end preInit

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
  given Typeable[IO[Unit]] = a => a match
    case b: IO[t] => Some(b.as(()).asInstanceOf[IO[Unit] & a.type])
    case _ => None

  def downloadImage(uri: String): CatsIO[Image] =
    EitherT.liftF(
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
    )
  end downloadImage

  def main(preInit : PreInit) : DesktopWidget[CatsIO, ApplicationRequest] =
    extension[Event](value : DesktopWidget[CatsIO, Event])
      def clip(shape : gui4s.core.geometry.Rect[Float] => Clip) : DesktopWidget[CatsIO, Event] =
        gui4s.desktop.kit.common.widgets.decorator.clip(value)(shape)
      end clip
    end extension

    gapPadding(
      initWidget(
        supervisor = preInit.globalSupervisor,
        raiseExternalEvent = preInit.raiseEvent
      )(
        name = "image",
        imageSource = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
        imageWidget = data => image[CatsIO, ApplicationRequest](data).clip(Shapes.roundedCorners(15f)),
        placeholder = text(preInit.shaper, preInit.globalTextCache)("Wait.", SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))),
      )
    )(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end ImageExample
