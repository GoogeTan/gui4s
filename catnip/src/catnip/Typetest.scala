package catnip

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
def optionTypeable[T : Typeable as original] : Typeable[Option[T]] =
  x => x match
    case Some(original(_)) =>
      Some(x.asInstanceOf[x.type & Option[T]])
    case None => Some(None.asInstanceOf[x.type & None.type])
    case _ => None
end optionTypeable


