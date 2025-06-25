package me.katze.gui4s.example
package api.exported

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import io.github.humbleui.skija.shaper.Shaper
import me.*
import me.katze.gui4s.example.draw.skija.SkijaBackend
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}

def skijaText[F[_] : Monad, PlaceError, DownEvent, Event](shaper : Shaper, ffi : FFI[F], text : String, style : SkijaTextStyle) : SkijaWidget[F, Float, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.skijaText[
    SkijaUpdateT[Float, Event],  SkijaPlaceT[F, Float, PlaceError], SkijaDraw[F, OglWindow], SkijaRecomposition[F], DownEvent, SkijaPlacedText
  ](
    skijaSizeText(ffi, text, shaper, style),
    drawText(ffi, _),
    ().pure[F],
  )
end skijaText

