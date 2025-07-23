package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.Monad
import cats.syntax.all.*
import me.*
import me.katze.gui4s.glfw.OglGlfwWindow
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}

def skijaText[
  IO[_] : Monad,
  UpdateError,
  PlaceError,
  MeasurementUnit,
  DownEvent,
  Event
](
  ffi : ForeighFunctionInterface[IO],
  textSizer : (String, SkijaTextStyle) => SkijaPlace[IO, MeasurementUnit, PlaceError, SkijaPlacedText],
  text : String,
  style : SkijaTextStyle,
) : SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.text[
    SkijaUpdateT[IO, UpdateError, MeasurementUnit, Event], SkijaPlaceT[IO, MeasurementUnit, PlaceError], SkijaDraw[IO, OglGlfwWindow[IO]], SkijaRecomposition[IO], DownEvent, SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    ().pure[IO],
  )
end skijaText

