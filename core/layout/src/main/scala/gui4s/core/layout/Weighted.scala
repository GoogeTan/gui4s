package gui4s.core.layout

enum Weighted[+T](val value: T):
  case Rigid(override val value: T) extends Weighted(value)
  case Weight(override val value: T, weight: Float) extends Weighted(value)

  def isRigid : Boolean = this match
      case Rigid(_) => true
      case _ => false
  end isRigid

  def as[B](value: B): Weighted[B] =
      this match
          case Rigid(_) => Rigid(value)
          case Weight(_, weight) => Weight(value, weight)
      end match
  end as

  def map[B](f : T => B) : Weighted[B] =
    this match
      case Weighted.Rigid(value) => Rigid(f(value))
      case Weighted.Weight(value, weight) => Weight(f(value), weight)

end Weighted

object Weighted:
  def apply[T](value : T, weight : Option[Float]) : Weighted[T] =
    weight.fold(Rigid(value))(Weight(value, _))
  end apply
end Weighted