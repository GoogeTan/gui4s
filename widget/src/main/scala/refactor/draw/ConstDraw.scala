package me.katze.gui4s.widget
package refactor.draw

def constDraw[T, Draw](draw : Draw) : Drawable[T, Draw] = _ => draw 