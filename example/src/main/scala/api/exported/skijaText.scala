package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.{Monad, Monoid}
import cats.syntax.all.*
import me.*
import me.katze.gui4s.glfw.{GlfwWindow, OglGlfwWindow}
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}
import me.katze.gui4s.widget.library.Widget

import scala.language.experimental.namedTypeArguments

def skijaText[
  IO[_] : Monad,
  Update[_] : Monad,
  RecompositionReaction : Monoid as RRM,
  HandlableEvent,
  Window,
  PlaceError,
  MeasurementUnit,
](
  ffi : ForeighFunctionInterface[IO],
  textSizer : SizeText[SkijaPlaceT[IO, MeasurementUnit, PlaceError]],
  text : String,
  style : SkijaTextStyle,
) : SkijaPlace[IO, MeasurementUnit, PlaceError, Widget[Update, SkijaPlaceT[IO, MeasurementUnit, PlaceError], SkijaDraw[IO, Window], RecompositionReaction, HandlableEvent]] =
  me.katze.gui4s.widget.library.text[
    Update,
    SkijaPlaceT[IO, MeasurementUnit, PlaceError],
    SkijaDraw[IO, Window],
    RecompositionReaction,
    HandlableEvent,
    SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    RRM.empty,
  )
end skijaText

