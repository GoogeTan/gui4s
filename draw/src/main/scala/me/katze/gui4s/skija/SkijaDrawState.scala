package me.katze.gui4s.skija

import io.github.humbleui.skija.{Canvas, DirectContext}

final case class SkijaDrawState[
  F[_],
  Window,
](
  context : DirectContext,
  window : Window,
  canvas : Canvas
)

