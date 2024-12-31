package me.katze.gui4s.draw
package stbtt

import cats.effect.Resource
import java.nio.ByteBuffer

final case class FontMetrics(ascent : Int, descent : Int, lineGap : Int)

trait Stbtt[F[_]]:
  type FontInfo
  type BakedCharBuffer
  
  def getFontMetrics(info : FontInfo) : F[FontMetrics] 

  def createFontInfo(ttf : ByteBuffer) : F[FontInfo]
  
  def bakeFont(ttf : ByteBuffer, fontHeight : Int, bitmapWidth : Int, bitmapHeight : Int) : Resource[F, BakedCharBuffer]
end Stbtt
