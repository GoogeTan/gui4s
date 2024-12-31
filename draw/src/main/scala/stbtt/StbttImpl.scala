package me.katze.gui4s.draw
package stbtt

import test.test2.stackPushResource

import cats.effect.kernel.{Resource, Sync}
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.glGenTextures
import org.lwjgl.stb.{STBTTBakedChar, STBTTFontinfo}
import org.lwjgl.stb.STBTruetype.{stbtt_BakeFontBitmap, stbtt_GetFontVMetrics, stbtt_InitFont}

import java.nio.ByteBuffer

final class StbttImpl[F[_] : Sync](using impure: Impure[F]) extends Stbtt[F]:
  override type FontInfo =  STBTTFontinfo
  override type BakedCharBuffer = STBTTBakedChar.Buffer

  override def getFontMetrics(info: STBTTFontinfo): F[FontMetrics] =
    stackPushResource.use:
      stack =>
        impure.impure {
          val pAscent = stack.mallocInt(1)
          val pDescent = stack.mallocInt(1)
          val pLineGap = stack.mallocInt(1)
          stbtt_GetFontVMetrics(info, pAscent, pDescent, pLineGap)
          val ascent = pAscent.get(0)
          val descent = pDescent.get(0)
          val lineGap = pLineGap.get(0)
          FontMetrics(ascent, descent, lineGap)
        }
  end getFontMetrics

  override def createFontInfo(ttf: ByteBuffer): F[STBTTFontinfo] =
    impure.impure:
      val info = STBTTFontinfo.create
      if !stbtt_InitFont(info, ttf) then
        throw new IllegalStateException("Failed to initialize font information.")
      info  
  end createFontInfo

  override def bakeFont(ttf: ByteBuffer, fontHeight : Int, bitmapWidth: Int, bitmapHeight: Int): Resource[F, STBTTBakedChar.Buffer] =
    Resource.make(
      impure.impure:
        val texID = glGenTextures
        val cdata = STBTTBakedChar.malloc(96)
        val bitmap = BufferUtils.createByteBuffer(bitmapWidth * bitmapHeight)
        stbtt_BakeFontBitmap(ttf, fontHeight, bitmap, bitmapWidth, bitmapHeight, 32, cdata) // TODO use returned information
        cdata
    )(
      cdata =>
        impure.impure:
          cdata.free()
    )
end StbttImpl
