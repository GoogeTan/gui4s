package me.katze.gui4s.example
package api.exported

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.*
import me.katze.gui4s.layout.{*, given}

def skijaText[F[+_] : {Monad}, Window](using backend: SkijaBackend[F, Window])(ffi : FFI[F], text : String, style : SkijaTextStyle) : Widget[F, Nothing, Any] =
  skijaText[
    SkijaUpdateT[Nothing],  SkijaPlaceT[F], SkijaDraw[F, OglWindow], Recomposition[F], Any, SkijaPlacedText
  ](
    sizeText(ffi, text, backend.globalShaper, style),
    drawText(ffi, _),
    ().pure[F]
  )
end skijaText

