package me.katze.gui4s.example
package api

enum MainAxisPlacementStrategy[+MU]:
  case Begin(gap : MU)
  case Center(gap : MU)
  case End(gap : MU)
  case SpaceBetween extends MainAxisPlacementStrategy[Nothing]
  case SpaceAround extends MainAxisPlacementStrategy[Nothing]
end MainAxisPlacementStrategy