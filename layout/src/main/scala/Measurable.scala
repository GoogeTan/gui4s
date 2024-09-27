package me.katze.gui4s.layout

import bound.Bounds

import io.github.iltotore.iron.{*, given}

trait Measurable[MU, +T]:
  def placeInside(constrains: Bounds[MU]): Sized[MU, T]
end Measurable
