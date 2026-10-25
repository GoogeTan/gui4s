package gui4s.core.widget.library.decorator

import cats.kernel.Monoid

/**
 * Декоратор виджета. Это можут быть отсупы по краям, фон, тень и т.д.
 * @tparam Widget Свободный виджет
 */
type Decorator[Widget] = Widget => Widget

object Decorator:
  given[Widget] : Monoid[Decorator[Widget]] with
    override def empty: Decorator[Widget] = identity

    override def combine(x: Decorator[Widget], y: Decorator[Widget]): Decorator[Widget] = x.andThen(y)
  end given
end Decorator

