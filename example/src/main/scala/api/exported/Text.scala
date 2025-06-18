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

def skijaText[F[+_] : {Monad}, Window](using backend: SkijaBackend[F, Window])(ffi : FFI[F], text : String, style : SkijaTextStyle) : Widget[F, Float, Nothing, Nothing, Any] =
  me.katze.gui4s.widget.library.skijaText[
    SkijaUpdateT[Nothing],  SkijaPlaceT[F, Nothing, Float], SkijaDraw[F, OglWindow], Recomposition[F], Any, SkijaPlacedText
  ](
    ???,//sizeText(ffi, text, backend.globalShaper, style),
    drawText(ffi, _),
    ().pure[F]
  )
end skijaText

