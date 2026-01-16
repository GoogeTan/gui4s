package gui4s.android.skia.paragraph

import org.jetbrains.skia.paragraph.*
import org.jetbrains.skia.*
import gui4s.core.geometry.Rect

def buildParagraph[F[_] : Sync](
                                 list : List[(String, TextStyle)],
                                 style : ParagraphStyle,
                                 fontCollection : FontCollection
                               ) : F[Paragraph] =
  paragraphBuilder(style, fontCollection).flatMap(
    builder =>
      buildBuilderF(
        addStyledTexts(list).run(builder)
      )
  )
end buildParagraph

def layout[F[_] : Sync as S](width : Float)(paragraph : Paragraph) : F[Unit] =
  S.delay(paragraph.layout(width))
end layout

def size[F[_] : Sync as S](paragraph : Paragraph) : F[Rect[Float]] =
  S.delay(Rect(paragraph.getMaxWidth, paragraph.getHeight))
end size