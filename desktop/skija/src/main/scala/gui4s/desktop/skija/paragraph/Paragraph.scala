package gui4s.desktop.skija
package paragraph

import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import io.github.humbleui.skija.paragraph.*

def buildParagraph[F[_] : Sync](
  list : List[(String, TextStyle)],
  style : ParagraphStyle,
  fontCollection : FontCollection
) : Resource[F, Paragraph] =
  paragraphBuilder(style, fontCollection).flatMap(
    builder =>
      buildBuilderF(
        list.foldMap(addStyledText).run(builder)
      )
  )
end buildParagraph

def layout[F[_] : Sync as S](width : Float)(paragraph : Paragraph) : F[Unit] =
  S.delay(paragraph.layout(width))
end layout

def size[F[_] : Sync as S](paragraph : Paragraph) : F[Rect[Float]] =
  S.delay(Rect(paragraph.getMaxWidth, paragraph.getHeight))
end size