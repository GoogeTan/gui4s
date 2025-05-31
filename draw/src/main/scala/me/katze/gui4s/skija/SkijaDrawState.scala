package me.katze.gui4s.skija

import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.glfw.Glfw

final case class SkijaDrawState[F[_], Window](context : DirectContext, glfw: Glfw[F, Window], window : Window, canvas : Canvas)

