package me.katze.gui4s.example
package api.impl

import api.EffectApi

import cats.Monad
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl

trait EffectApiImpl[
  F[+_], Draw,
  PlacementEffect[+_],
  WidgetTaskIn[+_],
  SystemEvent
](
  using val wl : WidgetLibraryImpl[F, Draw, PlacementEffect, WidgetTaskIn, SystemEvent]
) extends EffectApi[WidgetTaskIn]:
  override type Widget[+T] = wl.Widget[T]
  override type WidgetTask[+T] = wl.WidgetTask[T]

  override def sideEffect[T](name : String, task: WidgetTask[T]): Widget[T] =
    ???
  end sideEffect

  override def launchedEffect[T](name : String, keys: Any*)(task: WidgetTask[T]): Widget[T] =
    ???
  end launchedEffect
end EffectApiImpl
