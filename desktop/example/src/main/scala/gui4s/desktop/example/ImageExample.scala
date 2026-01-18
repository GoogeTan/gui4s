package gui4s.desktop.example

import catnip.syntax.all.given
import cats.Id
import cats.data.EitherT
import cats.effect._
import cats.effect.std._
import glfw4s.core._
import glfw4s.core.pure._
import glfw4s.jna.bindings.types._
import io.github.humbleui.skija.BlendMode

import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory

import gui4s.core.geometry._
import gui4s.core.layout.rowcolumn

import gui4s.desktop.kit._
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.skija.Font
import gui4s.desktop.skija.Image
import gui4s.desktop.skija.Paint
import gui4s.desktop.skija.SkijaTextStyle
import gui4s.desktop.skija.Typeface
import gui4s.desktop.skija.image.makeDeferredImageFromEncodedBytes
import gui4s.desktop.skija.shaper._
import gui4s.desktop.skija.typeface

object ImageExample extends UIApp:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

  val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s image example",
    width = 736/2,//TODO исправить то, что задается в пикселях, а не экранных координатах
    height = 920/2,
  )

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
          ).map(makeDeferredImageFromEncodedBytes)
      )
    )
  end downloadImage

  def inverseColorPaint : Paint =
    val inverseColor = new Paint
    inverseColor.setColor(0xFFAAAAAA)
    inverseColor.setBlendMode(BlendMode.DIFFERENCE)
    inverseColor.setAntiAlias(true)
    inverseColor
  end inverseColorPaint

  def headerTextStyle(typeface : Typeface): SkijaTextStyle =
    SkijaTextStyle(new Font(typeface, 72 * 2), inverseColorPaint)
  end headerTextStyle

  def pleaseWaitTextStyle(typeface : Typeface): SkijaTextStyle =
    SkijaTextStyle(new Font(typeface, 72 * 2), inverseColorPaint)
  end pleaseWaitTextStyle

  def main(
            glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
          ) : Resource[AppIO, DesktopWidget[AppIO, Nothing]] =
    given Ordering[Point2d[Float]] = Ordering.by[Point2d[Float], Float](_.x).orElse(Ordering.by[Point2d[Float], Float](_.y))

    for
      dispatcher <- Dispatcher.sequential[AppIO]
      supervisor <- Supervisor[AppIO]
      shaper <- createShaper[AppIO]
      cache: TextCache[AppIO] <- ScalacacheCache()
      text = TextWidget[AppIO](shaper, cache)
      resource = ResourceWidget(supervisor, eventBus.offer.andThen(liftCallbackIOToAppIO(_)))
      initialization = InitializationWidget(resource)

      typeface <- typeface.typefaceFromFile[AppIO]("OptimusPrinceps")

      centerPlacement = rowcolumn.OneElementPlacementStrategy.Center[OuterPlaceC[AppIO], Float]
      beginPlacement = rowcolumn.OneElementPlacementStrategy.Begin[OuterPlaceC[AppIO], Float, Float]

      textPlacement = rowcolumn.PlacementStrategy.PlaceIndependently[OuterPlaceC[AppIO], Rect[Float], List, Point2d[Float]](
        rowcolumn.PlacementStrategy.Zip[OuterPlaceC[AppIO], Float, Id, Float](
          Axis.Vertical,
          beginPlacement,
          beginPlacement
        ),
        Point2d(0f, 0f),
      )

      pleaseWaitPlacement =
        rowcolumn.PlacementStrategy.Zip[OuterPlaceC[AppIO], Float, Id, Float](
          Axis.Vertical,
          centerPlacement,
          centerPlacement
        )
    yield
      initialization(
        name = "image",
        effectToRun = downloadImage("https://i.pinimg.com/736x/c6/f2/41/c6f241cff25453bca4c861009e32d141.jpg"),
        body = image =>
          imageWidget[AppIO, Nothing](image)
            .withForeground(
              foreground = text("Princess Mononoke", headerTextStyle(typeface)),
              placement = textPlacement
            ),
        placeholder = text(
          "Please wait...",
          pleaseWaitTextStyle(typeface)
        )
      )
  end main
end ImageExample
