package me.katze.gui4s.example
package draw.skija

import draw.DrawApi

import cats.Monad
import cats.data.ReaderT
import cats.syntax.all.*
import me.katze.gui4s.glfw.Glfw
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds

def SkijaApi[F[_] : {Impure, Monad}, W](
                                          glfw : Glfw[F] { type Window = W },
                                          window : W,
                                        ) = DrawApi[F, Float, ReaderT[F, DrawState[W], Unit]](
  glfw.windowSize(window).map(size => new Bounds(size.width, size.height)),
  SkijaSimpleDrawApi[F, W](glfw)
)
