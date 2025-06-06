package me.katze.gui4s.example

import draw.skija.{*, given}
import impl.ENErrors
import place.MainAxisStrategyErrors

import catnip.FFI
import catnip.cats.effect.SyncFFI
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s.example.api.exported.{Widget, *}
import me.katze.gui4s.example.api.widget.skijaText
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.skija.SkijaTextStyle
import me.katze.gui4s.widget.EventReaction

object SkijaAppExample extends IOApp:
  given MainAxisStrategyErrors = ENErrors
  given ffi : FFI[IO] = SyncFFI[IO]

  override def run(args: List[String]): IO[ExitCode] =
    skijaApp[IO](
      widget = main, 
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread
    )
  end run

  def main(using SkijaBackend[IO, OglWindow]) : Widget[IO, Nothing, SkijaDownEvent] =
    skijaColumn[IO, Nothing, SkijaDownEvent](
      (0 until 6).toList.map(
        lineNumber =>
          skijaStateful[IO, SkijaDownEvent, Int, Nothing, Unit](
            "line-" + lineNumber.toString,
            1,
            (state, _) => EventReaction(state + 1, Nil, Nil),
            state => skijaText(
                ffi,
                "# line value " + state.toString,
                SkijaTextStyle(new Font(Typeface.makeDefault(), 26), new Paint().setColor(0xFF8484A4))
              ),
            _ => IO.unit
          )
      ),
      MainAxisPlacementStrategy.SpaceBetween, // TODO fix end gap
      AdditionalAxisPlacementStrategy.Center
    )
  end main
end SkijaAppExample
