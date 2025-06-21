package me.katze.gui4s.example
package api.exported

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.*
import me.katze.gui4s.example.draw.skija.SkijaBackend
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}

def skijaText[F[+_] : {Monad}, Window, PlaceError, DownEvent, Event](using backend: SkijaBackend[F, Window])(ffi : FFI[F], text : String, style : SkijaTextStyle) : SkijaWidget[F, Float, PlaceError, Event, DownEvent] =
  me.katze.gui4s.widget.library.skijaText[
    SkijaUpdateT[Event],  SkijaPlaceT[F, Float, PlaceError], SkijaDraw[F, OglWindow], SkijaRecomposition[F], DownEvent, SkijaPlacedText
  ](
    skijaSizeText(ffi, text, backend.globalShaper, style),
    drawText(ffi, _),
    ().pure[F],
  )
end skijaText

