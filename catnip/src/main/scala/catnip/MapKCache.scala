package catnip

import cats.syntax.all.*
import cats.{Applicative, Monad, ~>}

final class MapKCache[IO[_] : Applicative, GIO[_] : Monad, K, V](original : Cache[IO, K, V], f : IO ~> GIO) extends Cache[GIO, K, V]:
  override def get(key : K) : GIO[Option[V]] =
    f(original.get(key))
  end get

  override def getOrPut(key : K, value : GIO[V]) : GIO[V] =
    f(original.get(key)).flatMap:
      case Some(res) => res.pure
      case None =>
        value.flatMap:
          res =>
            f(original.getOrPut(key, res.pure))
  end getOrPut
end MapKCache
