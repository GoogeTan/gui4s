package catnip

/**
 * A type class that describes presence of new value.
 *
 * The easiest example is [[scala.Option]]: there might be a new value or not.
 *
 * Sometimes multiple ways of having new values are needed it is why this typeclass exists
 */
trait Patch[F[_]]:
  /**
   * Combines two patches.
   * @example For [[scala.Option]] it just returns newer value
   */
  def merge[T](older : F[T], newer : F[T]) : F[T]

  /**
   * Returns new value if present or an old one
   */
  def applyTo[T](oldValue : T, newValue : F[T]) : T

  /**
   * No new value present
   */
  def empty[T] : F[T]
end Patch

object Patch:
  given Patch[Option] with
    override def merge[T](older: Option[T], newer: Option[T]): Option[T] =
      newer.orElse(older)
    end merge

    override def applyTo[T](oldValue: T, newValue: Option[T]): T =
      newValue.getOrElse(oldValue)
    end applyTo

    override def empty[T]: Option[T] = None
  end given
end Patch
