package me.katze.gui4s.layout

trait Layout[MU, T]:
  def apply(elements : List[Placed[MU, T]]) : Sized[MU, T]
end Layout
