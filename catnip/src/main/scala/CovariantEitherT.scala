package catnip

final case class CovariantEitherT[+F[+_], +Error, +Value](run : F[Either[Error, Value]])
