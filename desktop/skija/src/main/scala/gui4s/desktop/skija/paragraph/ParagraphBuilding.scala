package gui4s.desktop.skija
package paragraph

import cats.Monad
import cats.Monoid
import cats.arrow.Arrow
import cats.data.Kleisli
import cats.effect.Sync
import cats.syntax.all._
import io.github.humbleui.skija.paragraph._

type ParagraphBuilding[F[_]] = Kleisli[F, ParagraphBuilder, ParagraphBuilder]

def paragraphBuilder[F[_] : Sync as S](
                                    style : ParagraphStyle,
                                    fontCollection : FontCollection
                                  ) : F[ParagraphBuilder] =
  S.delay(new ParagraphBuilder(style, fontCollection))
end paragraphBuilder

// TODO я не знаю, почему эта инстанция не создается сама
given paragraphBuildingIsAMonoid[F[_] : Monad]: Monoid[ParagraphBuilding[F]] = summon[Arrow[Kleisli[F, *, *]]].algebra

def pushStyle[F[_] : Sync as S](textStyle: TextStyle): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  Kleisli(builder => S.delay(builder.pushStyle(textStyle)))
end pushStyle

def addText[F[_] : Sync as S](text: String): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  Kleisli(builder => S.delay(builder.addText(text)))
end addText

def addStyledTexts[F[_] : Sync](texts: List[(String, TextStyle)]): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  texts
    .map(addStyledText)
    .foldLeft(Kleisli.ask)(_.andThen(_))
end addStyledTexts

def addStyledText[F[_] : Sync](text: String, style: TextStyle): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  pushStyle(style).andThen(addText(text))
end addStyledText

def buildBuilder[F[_] : Sync as S](builder: ParagraphBuilder): F[Paragraph] =
  S.delay(builder.build)
end buildBuilder

def buildBuilderF[F[_] : Sync](builder: F[ParagraphBuilder]): F[Paragraph] =
  builder >>= buildBuilder
end buildBuilderF
