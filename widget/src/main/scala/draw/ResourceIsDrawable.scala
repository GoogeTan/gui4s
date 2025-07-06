package me.katze.gui4s.widget
package draw

def resourceIsDrawable[Widget, Value, Draw](widgetIsDrawable : Drawable[Widget, Draw]) : Drawable[Resource[Widget, Value], Draw] =
  self => widgetIsDrawable(self.widget)
end resourceIsDrawable  
