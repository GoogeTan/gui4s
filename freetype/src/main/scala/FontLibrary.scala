package me.katze.gui4s.freetype

import cats.effect.Resource

import java.nio.ShortBuffer

final case class CharGlyph(width : Int, height : Int, brightnesses : ShortBuffer):
  def indexes : List[(Int, Int)] =
    (0 until width).flatMap(i => (0 until height).map(j => (i, j))).toList
  end indexes

  def get(i: Int, j: Int): Short =
    brightnesses.get(i * width + j)
  end get
end CharGlyph

trait FontLibrary[F[_]]:
  type Library
  type Font

  def initLibrary : Resource[F, Library]
  def loadFont(lib : Library, path : CharSequence) : Resource[F, Font]

  /**
   * Растеризует символ
   * @param font шрифт
   * @param fontHeight высота буквы в пикселях
   * @param char символ
   * @return Массив яркостей
   */
  def rasterChar(font : Font, fontHeight : Int, char: Char) : F[CharGlyph]
end FontLibrary
