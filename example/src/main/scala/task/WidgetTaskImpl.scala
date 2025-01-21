package me.katze.gui4s.example
package task


type WidgetTaskImpl[F[_], T] = (T => F[Unit]) => F[Unit]