package gui4s.desktop.example

import cats.data.EitherT
import cats.effect.*
import cats.effect.std.*
import glfw4s.core.*
import glfw4s.core.pure.PostInit
import glfw4s.jna.bindings.types.*
import gui4s.desktop.example.GridExample.AppIO
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.widget.library.decorator.*
import io.github.humbleui.skija.*
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder

import gui4s.desktop.skija.typeface.*
import scala.reflect.Typeable

object ImageExample extends UIApp:
  val settings = WindowCreationSettings(
    title = "Gui4s image example",
    width = 620,
    height = 480,
  )

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
  given Typeable[IO[Unit]] = a => a match
    case b: IO[t] => Some(b.as(()).asInstanceOf[IO[Unit] & a.type])
    case _ => None

  def downloadImage(uri: String): AppIO[Image] =
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

  def main(
            glfw: PostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
          ) : Resource[AppIO, DesktopWidget[AppIO, ApplicationRequest]] =
    extension[Event](value : DesktopWidget[AppIO, Event])
      def clip(shape : gui4s.core.geometry.Rect[Float] => Clip) : DesktopWidget[AppIO, Event] =
        gui4s.desktop.kit.widgets.decorator.clip(value)(shape)
      end clip
    end extension

    println("Test")

    for
      dispatcher <- Dispatcher.sequential[AppIO]
      supervisor <- Supervisor[AppIO]
      shaper <- createShaper[AppIO]
      cache: TextCache[AppIO] <- ScalacacheCache()
      typeface <- defaultTypeface[AppIO]
    yield gapPadding(
      initWidget(
        supervisor = supervisor,
        raiseExternalEvent = eventBus.offer.andThen(liftCallbackIOToAppIO(_))
      )(
        name = "image",
        imageSource = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
        imageWidget = data => image[AppIO, ApplicationRequest](data).clip(Shapes.round),
        placeholder = text(shaper, cache)("Wait.", SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))),
      )
    )(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end ImageExample
