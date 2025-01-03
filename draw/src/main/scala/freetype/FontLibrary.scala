package me.katze.gui4s.draw
package freetype

import cats.effect.Resource

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
  def renderChar(font : Font, fontHeight : Int, char: Char) : F[List[List[Int]]]
end FontLibrary
