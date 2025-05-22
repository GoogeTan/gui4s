package me.katze.gui4s.example

enum MainAxisPlacementStrategy[+MeasurementUnit]:
  case Begin(gap : MeasurementUnit)
  case Center(gap : MeasurementUnit)
  case End(gap : MeasurementUnit)
  case SpaceBetween extends MainAxisPlacementStrategy[Nothing]
  case SpaceAround extends MainAxisPlacementStrategy[Nothing]
end MainAxisPlacementStrategy