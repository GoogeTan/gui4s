package gui4s.desktop.example.cats

import catnip.Cache
import cats.effect.{IO, Resource}
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
  def apply[K <: AnyRef, V]() : Resource[IO, ScalacacheCache[IO, K, V]] =
    Resource.eval(CaffeineCache[IO, K, V]).map(new ScalacacheCache(_))
  end apply
end ScalacacheCache

