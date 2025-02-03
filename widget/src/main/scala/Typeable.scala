package me.katze.gui4s.widget

import stateful.RichTypeChecker
import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
given [T: Typeable]: Typeable[(T, T)] = (a : Any) => a match // Если заменить на краткую форму, почему-то не проходит тайп чекер
  case (b : Any, c : Any) =>
    for
      bb <- summon[Typeable[T]].unapply(b)
      cc <- summon[Typeable[T]].unapply(c)
    yield (bb, cc).asInstanceOf[(T, T) & a.type]
  case _ => None
end given

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.Any")) // Приложение должно падать точно, если тип не совпал
given[T](using TT : Typeable[T]): RichTypeChecker[T] = 
  (value : Any, errorText : String) => 
    TT.unapply(value).getOrElse[T](throw Exception(s"Cast failed. Expected $TT found $errorText", Exception(errorText)))
end given
