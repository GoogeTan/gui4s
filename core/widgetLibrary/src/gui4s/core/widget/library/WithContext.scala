package gui4s.core.widget.library

import cats.Applicative

/**
 * Декоратор, предоставляющий контекст данного типа.
 * @tparam Widget
 * @tparam Context
 */
type WithContext[Widget, +Context] =
  (Context => Widget) => Widget

given[Widget] : Applicative[WithContext[Widget, *]] with
  override def pure[A](x: A): WithContext[Widget, A] = _(x)

  override def ap[A, B](ff: WithContext[Widget, A => B])(fa: WithContext[Widget, A]): WithContext[Widget, B] =
    widget => ff(f => fa(a => widget(f(a))))
  end ap

  override def map[A, B](fa: WithContext[Widget, A])(f: A => B): WithContext[Widget, B] =
    widget => fa(f andThen widget)
  end map
end given