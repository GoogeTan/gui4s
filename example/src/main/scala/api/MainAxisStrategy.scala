package me.katze.gui4s.example
package api

enum MainAxisStrategy[+MU]:
  case Begin(gap : MU)
  case Center(gap : MU)
  case End(gap : MU)
  case SpaceBetween extends MainAxisStrategy[Nothing]
  case SpaceAround extends MainAxisStrategy[Nothing]
end MainAxisStrategy