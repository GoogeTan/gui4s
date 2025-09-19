package gui4s.desktop.skija
package paragraph

import catnip.ForeignFunctionInterface
import cats.arrow.Arrow
import cats.data.Kleisli
import cats.effect.{Resource, Sync}
import cats.syntax.all.*
import cats.{Monad, Monoid}
import io.github.humbleui.skija.paragraph.{FontCollection, Paragraph, ParagraphBuilder, ParagraphStyle, TextStyle}

type ParagraphBuilding[F[_]] = Kleisli[F, ParagraphBuilder, ParagraphBuilder]

object ParagraphBuilding:
  def paragraphBuilder[F[_] : {Sync, ForeignFunctionInterface as ffi}](
                                                                        style : ParagraphStyle,
                                                                        fontCollection : FontCollection
                                                                      ) : Resource[F, ParagraphBuilder] =
    Resource.fromAutoCloseable(ffi.delay(new ParagraphBuilder(style, fontCollection)))
  end paragraphBuilder
  
  // TODO я не знаю, почему эта инстанция не создается сама
  given paragraphBuildingIsAMonoid[F[_] : Monad]: Monoid[ParagraphBuilding[F]] = summon[Arrow[Kleisli[F, *, *]]].algebra

  def pushStyle[F[_] : {Sync, ForeignFunctionInterface as ffi}](textStyle: TextStyle): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
    Kleisli(builder => ffi(builder.pushStyle(textStyle)))
  end pushStyle

  def addText[F[_] : {Sync, ForeignFunctionInterface as ffi}](text: String): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
    Kleisli(builder => ffi(builder.addText(text)))
  end addText

  def addStyledText[F[_] : {Sync, ForeignFunctionInterface as ffi}](text: String, style: TextStyle): Kleisli[F, ParagraphBuilder, ParagraphBuilder] =
    pushStyle(style).andThen(addText(text))
  end addStyledText

  def buildBuilder[F[_] : {Sync, ForeignFunctionInterface as ffi}](builder: ParagraphBuilder): Resource[F, Paragraph] =
    Resource.fromAutoCloseable(ffi(builder.build))
  end buildBuilder

  def buildBuilderF[F[_] : {Sync, ForeignFunctionInterface as ffi}](builder: F[ParagraphBuilder]): Resource[F, Paragraph] =
    Resource.eval(builder) >>= buildBuilder
  end buildBuilderF
end ParagraphBuilding
