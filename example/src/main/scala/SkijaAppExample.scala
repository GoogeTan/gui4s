package me.katze.gui4s.example

import api.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}
import draw.skija.{*, given}
import impl.ENErrors
import place.MainAxisStrategyErrors

import cats.effect.{ExitCode, IO, IOApp}
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.cats.effect.IOImpure

object SkijaAppExample extends IOApp:
  given MainAxisStrategyErrors = ENErrors

  override def run(args: List[String]): IO[ExitCode] =
    skijaApp[IO](
      widget = main, 
      updateLoopExecutionContext = this.runtime.compute, 
      drawLoopExecutionContext = MainThread
    )
  end run

  def main(using SkijaBackend[IO, OglWindow]) : Widget[IO, Nothing] =
    skijaColumn(
      (0 until 15).toList.map(
        lineNumber =>
          skijaText("# line" + lineNumber.toString, new Font(Typeface.makeDefault(), 18), new Paint().setColor(0xFF8484A4)),
      ),
      MainAxisPlacementStrategy.Begin(0f),
      AdditionalAxisPlacementStrategy.Begin
    )
  end main
end SkijaAppExample
