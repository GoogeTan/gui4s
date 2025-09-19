package gui4s.desktop.skija
package shaper

import catnip.ForeignFunctionInterface
import cats.FlatMap
import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.{Font, TextBlob}
import io.github.humbleui.skija.shaper.Shaper

def createShaper[F[_] : {Sync, ForeignFunctionInterface as ffi}]: Resource[F, Shaper] =
  Resource.fromAutoCloseable(ffi(Shaper.make()))
end createShaper

def shape[F[_] : {FlatMap, ForeignFunctionInterface as ffi}](shaper : Shaper, text : String, font : Font) : F[TextBlob] =
  ffi(
    shaper.shape(text, font)
  )
end shape

def shape[F[_] : {FlatMap, ForeignFunctionInterface as ffi}](shaper : Shaper, text : String, font : Font, width : Float) : F[TextBlob] =
  ffi(
    shaper.shape(text, font, width)
  )
end shape

def shape[F[_] : {FlatMap, ForeignFunctionInterface as ffi}](shaper : Shaper, text : String, font : Font, maybeWidth : Option[Float]) : F[TextBlob] =
  maybeWidth match
    case Some(width) =>
      shape(shaper, text, font, width)
    case None =>
      shape(shaper, text, font)
  end match
end shape