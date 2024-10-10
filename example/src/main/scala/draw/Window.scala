package me.katze.gui4s.example
package draw


trait Window[+F[_], MU]:
  def size : F[(MU, MU)]
  def resize(width : MU, height : MU) : F[Unit]
  def enterFullScreen : F[Unit]
