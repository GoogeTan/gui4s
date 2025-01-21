package me.katze.gui4s.example
package task

trait Fiber[+F[_]]:
  def cancel : F[Unit]
  def finished : F[Boolean]
end Fiber
