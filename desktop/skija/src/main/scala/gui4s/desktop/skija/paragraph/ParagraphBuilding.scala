package gui4s.desktop.skija
package paragraph

import cats.arrow.Arrow
import cats.data.Kleisli
import cats.effect.{Resource, Sync}
import cats.syntax.all.*
import cats.{Monad, Monoid}
import io.github.humbleui.skija.paragraph.*

type ParagraphBuilding[F[_]] = Kleisli[F, ParagraphBuilder, ParagraphBuilder]

def paragraphBuilder[F[_] : Sync as S](
                                    style : ParagraphStyle,
                                    fontCollection : FontCollection
                                  ) : Resource[F, ParagraphBuilder] =
  Resource.fromAutoCloseable(S.delay(new ParagraphBuilder(style, fontCollection)))
end paragraphBuilder

// TODO я не знаю, почему эта инстанция не создается сама
given paragraphBuildingIsAMonoid[F[_] : Monad]: Monoid[ParagraphBuilding[F]] = summon[Arrow[Kleisli[F, *, *]]].algebra

def pushStyle[F[_] : Sync as S](textStyle: TextStyle): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  Kleisli(builder => S.delay(builder.pushStyle(textStyle)))
end pushStyle

def addText[F[_] : Sync as S](text: String): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  Kleisli(builder => S.delay(builder.addText(text)))
end addText

def addStyledText[F[_] : Sync](text: String, style: TextStyle): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
  pushStyle(style).andThen(addText(text))
end addStyledText

def buildBuilder[F[_] : Sync as S](builder: ParagraphBuilder): Resource[F, Paragraph] =
  Resource.fromAutoCloseable(S.delay(builder.build))
end buildBuilder

def buildBuilderF[F[_] : Sync](builder: F[ParagraphBuilder]): Resource[F, Paragraph] =
  Resource.eval(builder) >>= buildBuilder
end buildBuilderF
