package catnip
package syntax

import scala.reflect.Typeable

object typeable:
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
  given [T: Typeable as tpb]: Typeable[(T, T)] = (a: Any) => a match // Если заменить на краткую форму, почему-то не проходит тайп чекер
    case (b: Any, c: Any) =>
      for
        bb <- tpb.unapply(b)
        cc <- tpb.unapply(c)
      yield (bb, cc).asInstanceOf[(T, T) & a.type]
    case _ => None
  end given
end typeable
