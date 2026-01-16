package gui4s.desktop.example

import catnip.syntax.all.{*, given}
import cats.Id
import cats.data.EitherT
import cats.effect.*
import cats.effect.std.*
import catnip.syntax.all.{*, given}
import glfw4s.core.*
import glfw4s.core.pure.*
import glfw4s.jna.bindings.types.*
import gui4s.core.widget.library.decorator.Paddings
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn
import gui4s.desktop.kit.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.*
import io.github.humbleui.skija.*
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import gui4s.desktop.skija.typeface.*
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}
import gui4s.core.layout.rowcolumn
import gui4s.desktop.skija.typeface

import scala.reflect.Typeable

object ImageExample extends UIApp:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

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
            glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
          ) : Resource[AppIO, DesktopWidget[AppIO, ApplicationRequest]] =
    extension[Event](value : DesktopWidget[AppIO, Event])
      def clip(shape : gui4s.core.geometry.Rect[Float] => Clip) : DesktopWidget[AppIO, Event] =
        gui4s.desktop.kit.widgets.decorator.clip(value)(shape)
      end clip
    end extension

    val inverseColor = new Paint
    inverseColor.setColor(0xFFAAAAAA) // White for inversion

    inverseColor.setBlendMode(BlendMode.DIFFERENCE)
    inverseColor.setAntiAlias(true)

    given Ordering[Point2d[Float]] = Ordering.by[Point2d[Float], Float](_.x).orElse(Ordering.by[Point2d[Float], Float](_.y))

    for
      dispatcher <- Dispatcher.sequential[AppIO]
      supervisor <- Supervisor[AppIO]
      shaper <- createShaper[AppIO]
      cache: TextCache[AppIO] <- ScalacacheCache()
      typeface <- typeface.typefaceFromFile[AppIO]("OptimusPrinceps")
    yield
      initWidget[AppIO, ApplicationRequest, Image](
        supervisor = supervisor,
        raiseExternalEvent = eventBus.offer.andThen(liftCallbackIOToAppIO(_))
      )(
        name = "image",
        imageSource = downloadImage("https://i.pinimg.com/736x/c6/f2/41/c6f241cff25453bca4c861009e32d141.jpg"),
        imageWidget = data =>
          layersWidget[AppIO, ApplicationRequest](
            Nil,
            List(
              text[AppIO](shaper, cache)[ApplicationRequest]("Princess Mononoke", SkijaTextStyle(new Font(typeface, 72 * 2), inverseColor))
            ),
            rowcolumn.PlacementStrategy.PlaceIndependently[OuterPlaceC[AppIO], Rect[Float], List, Point2d[Float]](
              rowcolumn.PlacementStrategy.Zip[OuterPlaceC[AppIO], Float, Id, Float](
                Axis.Vertical,
                rowcolumn.OneElementPlacementStrategy.Begin[OuterPlaceC[AppIO], Float, Float],
                rowcolumn.OneElementPlacementStrategy.Begin[OuterPlaceC[AppIO], Float, Float],
              ),
              Point2d(0f, 0f),
            )
          )(
            image[AppIO, ApplicationRequest](data)
          ),
        placeholder = text[AppIO](shaper, cache)[ApplicationRequest]("Wait.", SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))),
      )
  end main
end ImageExample
