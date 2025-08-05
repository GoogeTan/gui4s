package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.Monad
import cats.syntax.all.*
import me.*
import me.katze.gui4s.glfw.{GlfwWindow, OglGlfwWindow}
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}

import scala.language.experimental.namedTypeArguments

def skijaText[
  IO[_] : Monad,
  UpdateError,
  PlaceError,
  MeasurementUnit,
  DownEvent,
  Event
](
  ffi : ForeighFunctionInterface[IO],
  textSizer : SizeText[SkijaPlaceT[IO, MeasurementUnit, PlaceError]],
  text : String,
  style : SkijaTextStyle,
) : SkijaWidget[IO, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.text[
    SkijaUpdateT[IO, MeasurementUnit, UpdateError, Event],
    SkijaPlaceT[IO, MeasurementUnit, PlaceError],
    SkijaDraw[IO, OglGlfwWindow],
    SkijaRecomposition[IO],
    DownEvent,
    SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    SkijaRecomposition.empty[IO],
  )
end skijaText

