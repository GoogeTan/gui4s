package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.Monad
import cats.syntax.all.*
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.given
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}

def skijaText[IO[_] : Monad, PlaceError, DownEvent, Event](
                                                            ffi : ForeighFunctionInterface[IO],
                                                            textSizer : (String, SkijaTextStyle) => SkijaPlace[IO, Float, PlaceError, SkijaPlacedText],
                                                            text : String,
                                                            style : SkijaTextStyle,
                                                          ) : SkijaWidget[IO, Float, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.text[
    SkijaUpdateT[IO, Float, Event],  SkijaPlaceT[IO, Float, PlaceError], SkijaDraw[IO, OglWindow], SkijaRecomposition[IO], DownEvent, SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    ().pure[IO],
  )
end skijaText

