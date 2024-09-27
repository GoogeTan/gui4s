package me.katze.gui4s.widget

import stateful.RichTypeChecker
import scala.reflect.Typeable

given [T: Typeable]: Typeable[(T, T)] = a => a match // Если заменить на краткую форму, почему-то не проходит тайп чекер
  case (b, c) =>
    for
      bb <- summon[Typeable[T]].unapply(b)
      cc <- summon[Typeable[T]].unapply(c)
    yield (bb, cc).asInstanceOf[(T, T) & a.type]
  case _ => None
end given

given [T: Typeable]: RichTypeChecker[T] = value => summon[Typeable[T]].unapply(value).toRight("Cast failed")
