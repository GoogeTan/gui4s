package me.katze.gui4s.example
package api.exported

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.effect.Sync
import cats.syntax.all.*
import cats.{Functor, Monad}
import io.github.humbleui.skija.shaper.Shaper
import me.*
import me.katze.gui4s.example.draw.skija.SkijaBackend
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}

def skijaText[IO[_] : Monad, PlaceError, DownEvent, Event](
                                                            shaper : Shaper,
                                                            ffi : FFI[IO],
                                                            textSizer : (String, SkijaTextStyle) => SkijaPlace[IO, Float, PlaceError, SkijaPlacedText],
                                                            text : String, 
                                                            style : SkijaTextStyle,
                                                          ) : SkijaWidget[IO, Float, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.text[
    SkijaUpdateT[Float, Event],  SkijaPlaceT[IO, Float, PlaceError], SkijaDraw[IO, OglWindow], SkijaRecomposition[IO], DownEvent, SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    ().pure[IO],
  )
end skijaText

