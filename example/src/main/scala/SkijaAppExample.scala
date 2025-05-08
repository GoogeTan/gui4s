package me.katze.gui4s.example

import api.*
import draw.skija.{*, given}

import cats.effect.{ExitCode, IO, IOApp}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.library.Empty
import me.katze.gui4s.widget.stateful.TaskFinished
import me.katze.gui4s.widget.{EventResult, given}

object SkijaAppExample extends IOApp:
  given Empty[Recomposition[IO]] with
    override def empty: Recomposition[IO] = IO.unit
  end given

  override def run(args: List[String]): IO[ExitCode] =
    skijaApp[IO](
      backend =>
        def text(value : String) =
          textWidget[
            EventResult,
            SkijaDraw[IO, OglWindow],
            MeasurableT[IO, Float],
            Recomposition[IO],
            Float,
            SkijaTextStyle,
            SkijaPlacedText,
            Shaper,
            TaskFinished
          ](value, backend.globalShaper, SkijaTextStyle(new Font(Typeface.makeDefault(), 18), new Paint().setColor(0xFF858585)))
        text("")
      ,
      this.runtime.compute,
      MainThread
    )
  end run
end SkijaAppExample