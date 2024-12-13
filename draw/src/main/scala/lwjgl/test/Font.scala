package me.katze.gui4s.draw
package lwjgl.test

import cats.effect.IO
import org.lwjgl.stb.STBTTFontinfo
import org.lwjgl.stb.STBTruetype.{stbtt_GetFontVMetrics, stbtt_InitFont}

import java.nio.ByteBuffer

final case class Font(ttf : ByteBuffer, info: STBTTFontinfo, ascent : Int, descent : Int, lineGap : Int)

object Font:
  def load(resource: String) : IO[Either[String, Font]] =
    for
      ttf <- ioResourceToByteBuffer(resource, 512 * 1024)
      info <- IO.apply(STBTTFontinfo.create)
      res <- initFont(info, ttf).ifM(
        ifTrue = getFontMetrics(info)
          .map(Font(ttf, info, _, _, _))
          .map(Right(_)),
        ifFalse = IO.pure(Left("Failed to initialize font information."))
      )
    yield res
  end load

  def initFont(info: STBTTFontinfo, ttf : ByteBuffer) : IO[Boolean] =
    IO.apply(stbtt_InitFont(info, ttf))
  end initFont

  def getFontMetrics(info: STBTTFontinfo): IO[(Int, Int, Int)] =
    stackPushResource.use:
      stack =>
        IO:
          val pAscent = stack.mallocInt(1)
          val pDescent = stack.mallocInt(1)
          val pLineGap = stack.mallocInt(1)
          stbtt_GetFontVMetrics(info, pAscent, pDescent, pLineGap)
          val ascent = pAscent.get(0)
          val descent = pDescent.get(0)
          val lineGap = pLineGap.get(0)
          (ascent, descent, lineGap)
  end getFontMetrics
end Font
