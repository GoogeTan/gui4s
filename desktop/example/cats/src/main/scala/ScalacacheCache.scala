package gui4s.desktop.example.cats

import catnip.Cache
import cats.effect.kernel.Sync
import cats.effect.Resource
import scalacache.caffeine.CaffeineCache

final class ScalacacheCache[IO[_], K, V](cache : scalacache.Cache[IO, K, V]) extends Cache[IO, K, V]:
  override def get(key: K): IO[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key : K, value : IO[V]) : IO[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache

object ScalacacheCache:
  def apply[F[_] : Sync, K <: AnyRef, V]() : Resource[F, ScalacacheCache[F, K, V]] =
    Resource.eval(CaffeineCache[F, K, V]).map(new ScalacacheCache(_))
  end apply
end ScalacacheCache

