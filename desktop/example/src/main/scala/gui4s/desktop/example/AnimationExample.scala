package gui4s.desktop.example

import catnip.*
import catnip.syntax.all.{*, given}
import cats.data.EitherT
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import glfw4s.core.*
import glfw4s.core.pure.*
import glfw4s.jna.bindings.types.*
import gui4s.core.geometry.Point2d
import gui4s.core.widget.LaunchedEffect
import gui4s.core.widget.library.animation.{Animation, SpringAnimation, TweenAnimation, Easing}
import gui4s.core.widget.library.animation.NormedVectorSpace.numericNormedVectorSpace
import gui4s.core.widget.library.decorator.Paddings
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.skija.typeface.*
import gui4s.desktop.widget.library.LaunchedEffectWidget
import io.github.humbleui.skija.*
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.reflect.Typeable
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory


object AnimationExample extends UIApp:
  given logging: LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: SelfAwareStructuredLogger[IO] = logging.getLogger

  val settings = WindowCreationSettings(
    title = "Gui4s animation example",
    width = 620,
    height = 480,
  )

  given Typeable[IO[Unit]] = a => a match
    case b: IO[t] => Some(b.as(()).asInstanceOf[IO[Unit] & a.type])
    case _ => None

  def updaterWidget[Event](
                            launchedEffect: LaunchedEffectWidget[DesktopWidget[AppIO, Event], Unit, AppIO[Unit]],
                            pushUpdate : AppIO[Unit]
                          ) : Decorator[DesktopWidget[AppIO, Event]] =
    original =>
      launchedEffect("updater", original, (), pushUpdate)
  end updaterWidget

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

    for
      dispatcher <- Dispatcher.sequential[AppIO]
      supervisor <- Supervisor[AppIO]
      shaper <- createShaper[AppIO]
      cache: TextCache[AppIO] <- ScalacacheCache()
      typeface <- defaultTypeface[AppIO]
      stateful = statefulWidget[AppIO]
      clickSource <- clickEventSource(window, glfw, eventBus).eval
      onClick = [Event] => (event : Event) =>
        clickCatcher(glfw.getCursorPos(window).map((x, y) => Point2d(x.toFloat, y.toFloat)), event, clickSource)
      animation = animationWidget[AppIO, Unit, Float]()
      _ <- supervisor.supervise(
        liftCallbackIOToAppIO(
          (eventBus.offer(DownEvent.WindowShouldBeRedrawn)
            *> IO.sleep(FiniteDuration(15, TimeUnit.MILLISECONDS))
            ).iterateWhile(_ => true)
        )
      ).eval
      floatAnimation : Animation[Float, Duration] = TweenAnimation(
        easing = Easing.Linear,//TODO fix me
        duration = Duration(300, TimeUnit.MILLISECONDS)
      )
    yield gapPadding[AppIO, ApplicationRequest](
      Paddings(10f, 10f, 10f, 10f)
    )(
      initWidget(
        supervisor = supervisor,
        raiseExternalEvent = eventBus.offer.andThen(liftCallbackIOToAppIO(_))
      )(
        name = "image",
        imageSource = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
        imageWidget = data =>
          stateful[Int, ApplicationRequest, Unit](
            name = "counter",
            initialState = 0,
            eventHandler = (state, _, events) => (state + events.size).pure[UpdateC[AppIO, ApplicationRequest]],
            body = count =>
              animation(
                name = "animation",
                targetValue = count.toFloat * 50,
                animation = floatAnimation,
                body = cornerRadius =>
                  onClick(())(
                    gui4s.desktop.kit.widgets.image[AppIO, Unit](data).clip(Shapes.roundedCorners(cornerRadius))
                  )
              ),
          ),
        placeholder = text(shaper, cache)("Wait.", SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))),
      )
    )
  end main
end AnimationExample

