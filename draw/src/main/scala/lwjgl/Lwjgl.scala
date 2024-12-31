package me.katze.gui4s.draw
package lwjgl

trait Lwjgl[F[_]]:
  def createCapabilities : F[Unit]
  
end Lwjgl
  
