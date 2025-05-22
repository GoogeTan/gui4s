package me.katze.gui4s.widget
package draw

type Drawable[-T, +Draw] = (self : T) => Draw
