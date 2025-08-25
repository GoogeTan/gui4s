package gui4s.core.widget
package draw

def constDraw[T, Draw](draw : Draw) : Drawable[T, Draw] = _ => draw