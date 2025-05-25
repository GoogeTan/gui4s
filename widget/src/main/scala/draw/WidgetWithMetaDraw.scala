package me.katze.gui4s.widget
package draw

def widgetWithMetaIsDrawable[Draw, Widget, Meta](initial : Drawable[Widget, Draw], adjustToMetaDraw : (Draw, Meta) => Draw) : Drawable[(Widget, Meta), Draw] =
  (self, meta) =>
    adjustToMetaDraw(
      initial(self),
      meta
    )
end widgetWithMetaIsDrawable
