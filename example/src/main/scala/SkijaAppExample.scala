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
    skijaApp[IO](main, this.runtime.compute, MainThread)
  end run

  def main(using SkijaBackend[IO, OglWindow]) =
    skijaColumn(
      (0 until 10).toList.map(
        lineNumber =>
          skijaText("# line" + lineNumber.toString, new Font(Typeface.makeDefault(), 100), new Paint().setColor(0xFF858585)),
      ),
      MainAxisPlacementStrategy.Begin(10f),
      AdditionalAxisPlacementStrategy.Center
    )
  end main
end SkijaAppExample
