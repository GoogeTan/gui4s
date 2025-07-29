package catnip

final case class CovariantOptionT[+F[+_], +Value](run : F[Option[Value]])

