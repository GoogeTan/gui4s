package me.katze.gui4s.example
package api.widget

import api.{*, given}
import draw.skija.*

import catnip.FFI
import catnip.syntax.all.given
import cats.syntax.all.*
import cats.{Functor, Monad}
import me.*
import me.katze.gui4s.example.api.exported.{Recomposition, Update, Widget}
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.skija.{*, given}

def skijaText[F[+_] : {Monad}, Window](using backend: SkijaBackend[F, Window])(ffi : FFI[F], text : String, style : SkijaTextStyle) : Widget[F, Nothing, Any] =
  skijaText[
    Update[Nothing],  MeasurableT[F, Float], SkijaDraw[F, OglWindow], Recomposition[F], Any, SkijaPlacedText
  ](
    sizeText(ffi, text, backend.globalShaper, style),
    drawText(ffi, _),
    ().pure[F]
  )
end skijaText

def skijaText[
  Update[+_] : Monad,
  Place[+_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  PlacedText
](
   text : Place[PlacedText],
   draw : PlacedText => Draw,
   emptyRecomposition : RecompositionReaction
 ) : Place[
  SkijaWidget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]
] =
  drawOnlyWidget[Update, Place, Draw, RecompositionReaction, HandleableEvent](text.map(draw), emptyRecomposition)
end skijaText

