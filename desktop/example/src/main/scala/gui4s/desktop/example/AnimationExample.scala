package gui4s.desktop.example

import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

import catnip._
import catnip.syntax.all.given
import cats.data.EitherT
import cats.effect._
import cats.effect.std._
import cats.syntax.all._
import glfw4s.core._
import glfw4s.core.pure._
import glfw4s.jna.bindings.types._
import io.github.humbleui.skija._

import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j.Slf4jFactory

import gui4s.core.geometry.Point2d
import gui4s.core.widget.library.animation.Animation
import gui4s.core.widget.library.animation.Easing
import gui4s.core.widget.library.animation.NormedVectorSpace.numericNormedVectorSpace
import gui4s.core.widget.library.animation.TweenAnimation
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.Paddings

import gui4s.desktop.kit._
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.skija._
import gui4s.desktop.skija.typeface._
import gui4s.desktop.widget.library.LaunchedEffectWidget


object AnimationExample extends UIApp:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

  val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s animation example",
    width = 620,
    height = 480,
  )

  def updaterWidget[Event](
                            launchedEffect: LaunchedEffectWidget[DesktopWidget[Event], Unit, IO[Unit]],
                            pushUpdate : IO[Unit]
                          ) : Decorator[DesktopWidget[Event]] =
    original =>
      launchedEffect("updater", original, (), pushUpdate)
  end updaterWidget

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

  def main(
    glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
    window: GLFWwindow,
    eventBus: Queue[IO, DownEvent],
  ) : Resource[IO, DesktopWidget[Nothing]] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      supervisor <- Supervisor[IO]
      shaper <- createShaper[IO]
      cache: TextCache[IO] <- ScalacacheCache()
      typeface <- defaultTypeface[IO]
      stateful = statefulWidget
      clickSource <- clickEventSource(window, glfw, eventBus).eval
      onClick = [Event] => (event : Event) =>
        clickCatcher(glfw.getCursorPos(window).map((x, y) => Point2d(x.toFloat, y.toFloat)), event, clickSource)
      animation = animationWidget[Unit, Float]()
      _ <- supervisor.supervise(
        (eventBus.offer(DownEvent.WindowShouldBeRedrawn)
          *> IO.sleep(FiniteDuration(15, TimeUnit.MILLISECONDS))
          ).iterateWhile(_ => true)
      ).eval
      floatAnimation : Animation[Float, Duration] = TweenAnimation(
        easing = Easing.Linear,
        duration = Duration(300, TimeUnit.MILLISECONDS)
      )
      resource = ResourceWidget(
        supervisor = supervisor,
        raiseExternalEvent = eventBus.offer
      )
      initialization = InitializationWidget(resource)
      text = TextWidget(shaper, cache)
    yield initialization(
      name = "image",
      effectToRun = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
      body = data =>
        stateful[Int, Nothing, Unit](
          name = "counter",
          initialState = 0,
          eventHandler = (state, _, events) => (state + events.size).pure[UpdateC[IO, Nothing]],
          body = count =>
            animation(
              name = "animation",
              targetValue = (count % 4).toFloat * 50,
              animation = floatAnimation,
              body = cornerRadius =>
                onClick(())(
                  imageWidget[Unit](data).clip(Shapes.roundedCorners(cornerRadius))
                )
            ),
        ),
      placeholder = text("Please, wait.", SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))),
    ).padding(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end AnimationExample

