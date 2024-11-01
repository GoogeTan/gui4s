package me.katze.gui4s.example

trait RunRecomposition[-TS, +F, -W]:
  def run(taskSet : TS, oldWidget : W, newWidget : W) : F
end RunRecomposition
