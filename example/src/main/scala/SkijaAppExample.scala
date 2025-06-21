package me.katze.gui4s.example

import impl.ENErrors
import place.MainAxisStrategyErrors

import catnip.FFI
import catnip.cats.effect.SyncFFI
import catnip.syntax.all.{*, given}
import cats.effect.{ExitCode, IO, IOApp}
import cats.data.EitherT
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s.example.api.decorator.handleClick
import me.katze.gui4s.example.api.exported.{*, given}
import me.katze.gui4s.example.draw.skija.SkijaBackend
import me.katze.gui4s.example.update.ApplicationRequest
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.skija.{SkijaDraw, SkijaTextStyle}
import me.katze.gui4s.widget.{EventReaction, Path}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}

import scala.annotation.experimental

@experimental
object SkijaAppExample extends IOApp:
  given MainAxisStrategyErrors = ENErrors
  given ffi : FFI[IO] = SyncFFI[IO]

  override def run(args: List[String]): IO[ExitCode] =
    skijaApp[IO, String](
      widget = main, 
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      runEitherTError = [V] => (value : EitherT[IO, String, V]) =>
        value.value.flatMap:
          case Left(error) =>
            IO.raiseError(new Exception(error))
          case Right(value) =>
            IO.pure(value)
    )
  end run

  def main(using SkijaBackend[IO, OglWindow]) : SkijaWidget[IO, Float, String, ApplicationRequest, SkijaDownEvent] =
    skijaColumn[IO, String, ApplicationRequest, SkijaDownEvent](
      (0 until 6).toList.map(
        lineNumber =>

            skijaStateful[IO, String, Float, SkijaDownEvent, Int, ApplicationRequest, Unit](
              "line-" + lineNumber.toString,
              1,
              (state, _) => EventReaction(state + 1, Nil, Nil),
              state =>
                handleClick[
                  SkijaUpdate[Float, Unit, *],
                  SkijaPlaceInnerT[IO, Float, String],
                  SkijaDraw[IO, OglWindow],
                  SkijaRecomposition[IO],
                  SkijaDownEvent,
                  Float
                ](
                  markEventHandled,
                  getCoordinates,
                  ???,
                )(
                  skijaText(
                    ffi,
                    "# line value " + state.toString,
                    SkijaTextStyle(new Font(Typeface.makeDefault(), 26), new Paint().setColor(0xFF8484A4))
                  )
                )(
                  (_, _) => raiseEvents[Float, Unit](List(()))
                ),
              _ => IO.unit,
              (value : Any, path : Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "] while expected Int"
            )
      ),
      MainAxisPlacementStrategy.SpaceBetween, // TODO fix end gap
      AdditionalAxisPlacementStrategy.Center
    )
  end main
end SkijaAppExample
