package gui4s.desktop.skija

package object paragraph {
  type Paragraph = io.github.humbleui.skija.paragraph.Paragraph
  type FontCollection = io.github.humbleui.skija.paragraph.FontCollection
  type ParagraphBuilder = io.github.humbleui.skija.paragraph.ParagraphBuilder
  type ParagraphStyle = io.github.humbleui.skija.paragraph.ParagraphStyle
  type TextStyle = io.github.humbleui.skija.paragraph.TextStyle
  type TextStyleAttribute = io.github.humbleui.skija.paragraph.TextStyleAttribute
  type RectHeightMode = io.github.humbleui.skija.paragraph.RectHeightMode
  type RectWidthMode = io.github.humbleui.skija.paragraph.RectWidthMode
  type TextBox = io.github.humbleui.skija.paragraph.TextBox
  type DecorationStyle = io.github.humbleui.skija.paragraph.DecorationStyle
  type DecorationLineStyle = io.github.humbleui.skija.paragraph.DecorationLineStyle

  object DecorationLineStyle:
    val SOLID : DecorationLineStyle = io.github.humbleui.skija.paragraph.DecorationLineStyle.SOLID
  end DecorationLineStyle

  object RectHeightMode:
    val MAX : RectHeightMode = io.github.humbleui.skija.paragraph.RectHeightMode.MAX
  end RectHeightMode

  object RectWidthMode:
    val TIGHT : RectWidthMode = io.github.humbleui.skija.paragraph.RectWidthMode.TIGHT
  end RectWidthMode

  object TextStyleAttribute:
    val ALL_ATTRIBUTES : TextStyleAttribute = io.github.humbleui.skija.paragraph.TextStyleAttribute.ALL_ATTRIBUTES
  end TextStyleAttribute
}
