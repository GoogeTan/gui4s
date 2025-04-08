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

@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class SkijaGui4sApp extends Gui4sApp[MeasurableT[IO, Float], Update[Task[Any]], Recomposition, Task, Float, SkijaDraw[IO, OglWindow], Any](
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
  MeasurableRunPlacement(_),
  a => a,
  [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO]
)

