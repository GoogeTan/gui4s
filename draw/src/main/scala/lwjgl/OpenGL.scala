package me.katze.gui4s.draw
package lwjgl

import java.nio.ShortBuffer

trait OpenGL[F[_]]:
  type BrightnessTexture

  def loadBrightnessTexture(width : Int, height : Int, data : ShortBuffer) : F[BrightnessTexture]

  def renderBrightnessTexture(brightnessTexture: BrightnessTexture) : F[Unit]
end OpenGL
  
