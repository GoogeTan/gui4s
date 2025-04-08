package me.katze.gui4s.example
package draw.skija

import api.impl.{DrawMonad, LayoutPlacementMeta}
import draw.skija.given
import draw.{DrawApi, SimpleDrawApi, TextStyle}
import impl.{*, given}

import cats.data.ReaderT
import cats.effect.IO
import cats.syntax.all.*
import cats.{Applicative, Monad}
import me.katze.gui4s.glfw.Glfw
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.TestFuncs
import me.katze.gui4s.widget.library.{LabelDraw, LayoutDraw}

def SkijaApi[F[_] : {Impure, Monad}, W](
                                          glfw : Glfw[F] { type Window = W },
                                          window : W,
                                        ) = DrawApi[F, Float, ReaderT[F, SkijaDrawState[W], Unit]](
  glfw.windowSize(window).map(size => new Bounds(size.width, size.height)),
  SkijaSimpleDrawApi[F, W](glfw)
)
  
given skijaLayoutDraw[F[_] : {Impure, Monad}, Window]: LayoutDraw[SkijaDraw[F, Window], LayoutPlacementMeta[Float]] =
  layoutDrawImpl[SkijaDraw[F, Window], Float]

given skijaTextDraw[Window, MeasurementUnit, Draw](using api : SimpleDrawApi[MeasurementUnit, Draw]): LabelDraw[Draw, LayoutPlacementMeta[MeasurementUnit]] =
  (text, meta) =>
    api.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
