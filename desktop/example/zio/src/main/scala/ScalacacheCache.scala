
package gui4s.desktop.example.zio

import catnip.Cache
import zio.Task

final class ScalacacheCache[K, V](cache: scalacache.Cache[Task, K, V]) extends Cache[Task, K, V] :
  override def get(key: K): Task[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key: K, value: Task[V]): Task[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache


object ScalacacheCache:
  def apply[K, V](cache: scalacache.Cache[Task, K, V]): ScalacacheCache[K, V] =
    new ScalacacheCache(cache)
end ScalacacheCache
