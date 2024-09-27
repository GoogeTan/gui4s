package me.katze.gui4s.example
package place


trait ApplicationBounds[+F[_], Bounds]:
  def currentBounds : F[Bounds]
end ApplicationBounds
