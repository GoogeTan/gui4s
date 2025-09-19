package gui4s.desktop.skija
package paragraph

import paragraph.ParagraphBuilding.{buildBuilderF, paragraphBuilder, given}

import catnip.*
import cats.*
import cats.effect.*
import cats.syntax.all.*
import io.github.humbleui.skija.paragraph.*

def buildParagraph[F[_] : {Sync, ForeignFunctionInterface as ffi}](
  list : List[(String, TextStyle)],
  style : ParagraphStyle,
  fontCollection : FontCollection
) : Resource[F, Paragraph] =
  paragraphBuilder(style, fontCollection).flatMap(
    builder =>
      buildBuilderF(
        list.foldMap(ParagraphBuilding.addStyledText).run(builder)
      )
  )
end buildParagraph