package me.katze.gui4s.example

import api.Widget
import api.widget.*
import draw.skija.{*, given}
import impl.ENErrors
import place.MainAxisStrategyErrors

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
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

  def main(using SkijaBackend[IO, OglWindow]) : Widget[IO, Nothing, TaskFinished] =
    skijaColumn[IO, Nothing, TaskFinished](
      (0 until 6).toList.map(
        lineNumber =>
          skijaText("# line" + lineNumber.toString, SkijaTextStyle(new Font(Typeface.makeDefault(), 26), new Paint().setColor(0xFF8484A4))),
      ),
      MainAxisPlacementStrategy.Begin(0), // TODO fix end gap
      AdditionalAxisPlacementStrategy.Center
    )
  end main
end SkijaAppExample
