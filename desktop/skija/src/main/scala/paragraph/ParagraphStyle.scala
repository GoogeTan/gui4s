package gui4s.desktop.skija
package paragraph

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.paragraph.ParagraphStyle

def makeParagraphStyle[F[_] : Sync as S] : Resource[F, ParagraphStyle] =
  Resource.fromAutoCloseable(
    S.delay(
      new ParagraphStyle
    )
  )
end makeParagraphStyle

