package me.katze.gui4s.example

import draw.*
import draw.skija.*
import impl.{ENErrors, WindowResized, containerPlacementCurried}
import place.RunPlacement
import task.{EventProducingEffectT, RunnableIO}
import update.ApplicationRequest

import cats.effect.IO
import cats.syntax.all.*
import cats.{Applicative, Monad}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.cats.effect.{IOImpure, given}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.{Measurable, MeasurableT, given}
import me.katze.gui4s.widget.{EventResult, given}

final case class SkijaMeasurable[F[_], MU, T](placeInside : Bounds[MU] => F[T])
type SkijaMeasurableT[F[_], MU] = [T] =>> SkijaMeasurable[F, MU, T]

final class SkijaMeasurableRunPlacement[F[_] : Monad, MeasurementUnit : Numeric](bounds: F[Bounds[MeasurementUnit]]) extends RunPlacement[F, SkijaMeasurableT[F, MeasurementUnit]]:
  override def run[T](toPlace: SkijaMeasurable[F, MeasurementUnit, T]): F[T] =
    bounds
      .flatMap(bounds => toPlace.placeInside(bounds))
  end run
end SkijaMeasurableRunPlacement

@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class SkijaGui4sApp extends Gui4sApp[SkijaMeasurableT[IO, Float], Update[Task[Any]], Recomposition, Task, Float, SkijaDraw[IO, OglWindow], Any](
  queue => 
    SkijaSimpleDrawApi.createForTests.map((api, glfw, window, rt, shaper) =>
      (
        glfw.windowSize(window).map(a => new Bounds(a.width, a.height)),
        simpleGraphicsDrawLoop[IO, Float, SkijaDraw[IO, OglWindow]](api, draw => draw.run(SkijaDrawState(rt.directContext, window, rt.canvas, shaper))),
        ???,
        ???
      )
    ),
  containerPlacementCurried(ENErrors),
  SkijaMeasurableRunPlacement(_),
  a => a,
  [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO]
)

