package me.katze.gui4s.widget.library
package decorator

import cats.kernel.Monoid

type Decorator[Widget] = Widget => Widget

object Decorator:
  given[Widget] : Monoid[Decorator[Widget]] with
    override def empty: Decorator[Widget] = identity

    override def combine(x: Decorator[Widget], y: Decorator[Widget]): Decorator[Widget] = x.andThen(y)
  end given
end Decorator

