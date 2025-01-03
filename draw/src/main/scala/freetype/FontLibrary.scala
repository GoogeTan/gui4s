package me.katze.gui4s.draw
package freetype

import cats.effect.Resource

final case class CharGlyph(width : Int, height : Int, brightnesses : Array[Byte]):
  def indexes : List[(Int, Int)] =
    (0 until width).flatMap(i => (0 until height).map(j => (i, j))).toList
  end indexes
  
  def get(i : Int, j : Int) : Byte =
    brightnesses(i * width + j)
  end get
end CharGlyph

trait FontLibrary[F[_]]:
  type Library
  type Font

  def initLibrary : Resource[F, Library]
  def loadFont(lib : Library, path : CharSequence) : Resource[F, Font]

  /**
   * Рисует символ
   * @param font шрифт
   * @param fontHeight высота буквы в пикселях
   * @param char символ
   * @return Массив яркостей
   */
  def renderChar(font : Font, fontHeight : Int, char: Char) : F[CharGlyph]
end FontLibrary
