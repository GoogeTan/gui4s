package me.katze.gui4s.widget

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
given [T: Typeable as tpb]: Typeable[(T, T)] = (a : Any) => a match // Если заменить на краткую форму, почему-то не проходит тайп чекер
  case (b : Any, c : Any) =>
    for
      bb <- tpb.unapply(b)
      cc <- tpb.unapply(c)
    yield (bb, cc).asInstanceOf[(T, T) & a.type]
  case _ => None
end given

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.Any")) // Приложение должно падать точно, если тип не совпал
given[T : Typeable as TT]: RichTypeChecker[T] = 
  (value : Any) => 
    TT.unapply(value)
end given
