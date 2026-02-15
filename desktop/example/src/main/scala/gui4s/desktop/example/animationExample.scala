package gui4s.desktop.example

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.widget.library.animation.NormedVectorSpace.numericNormedVectorSpace
import gui4s.core.widget.library.animation.{Animation, AnimationWidget, Easing, TweenAnimation}
import gui4s.core.widget.library.decorator.Paddings
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.*
import io.github.humbleui.skija.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

def animationExample(
  initialization: InitializationWidget,
  onClick : ClickCatcher,
  animation : AnimationWidget[DesktopWidget[Unit], Float, Duration],
  text: TextWidget,
  typeface : Typeface
) : DesktopWidget[Nothing] =
  val floatAnimation : Animation[Float, Duration] = TweenAnimation(
    easing = Easing.Linear,
    duration = Duration(300, TimeUnit.MILLISECONDS)
  )
  initialization(
    name = "image",
    effectToRun = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
    body = data =>
      statefulWidget[Int, Nothing, Unit](
        name = "counter",
        initialState = 0,
        eventHandler = (state, _, events) => (state + events.size).pure[UpdateC[IO, Nothing]],
        body = count =>
          animation(
            name = "animation",
            targetValue = (count % 4).toFloat * 50,
            animation = floatAnimation,
            body = cornerRadius =>
              onClick[Unit](())(
                imageWidget[Unit](data).clip(Shapes.roundedCorners(cornerRadius))
              )
          )
      ),
    placeholder = text("Please, wait.", SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))),
  ).padding(
    Paddings(10f, 10f, 10f, 10f)
  )
end animationExample

