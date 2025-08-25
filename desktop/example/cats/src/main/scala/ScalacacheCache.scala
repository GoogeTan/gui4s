package gui4s.desktop.example.cats

import catnip.Cache

final class ScalacacheCache[IO[_], K, V](cache :  scalacache.Cache[IO, K, V]) extends Cache[IO, K, V]:
  override def get(key: K): IO[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key : K, value : IO[V]) : IO[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache
