package me.katze.gui4s.draw
package lwjgl

import impure.Impure

import org.lwjgl.opengl.GL11.*

import java.nio.{IntBuffer, ShortBuffer}

class LwjglImpl[F[_]](impure: Impure[F]) extends Lwjgl[F]:
  override type BrightnessTexture = Int

  override def loadBrightnessTexture(width: Int, height: Int, data: ShortBuffer): F[BrightnessTexture] =
    impure.impure:
      val textureBuffer = IntBuffer.allocate(1)
      glGenTextures(textureBuffer)
      val textureId = textureBuffer.get(0)
      glBindTexture(GL_TEXTURE_2D, textureId)
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
      glTexImage2D(GL_TEXTURE_2D, 0, GL_R, width, height, 0, GL_RED, GL_UNSIGNED_BYTE, data)

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
      textureId
  end loadBrightnessTexture

  override def renderBrightnessTexture(brightnessTexture: BrightnessTexture): F[Unit] =
    impure.impure:
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, brightnessTexture)
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)

      glBegin(GL_QUADS)

      glTexCoord2f(0, 0)
      glVertex2i(-1, -1)

      glTexCoord2f(1, 0)
      glVertex2i(1, -1)

      glTexCoord2f(1, 1)
      glVertex2i(1, 1)

      glTexCoord2f(0, 1)
      glVertex2i(-1, 1)

      glEnd()
  end renderBrightnessTexture
end LwjglImpl

