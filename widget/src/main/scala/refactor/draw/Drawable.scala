package me.katze.gui4s.widget
package refactor.draw

type Drawable[-T, +Draw] = (self : T) => Draw
