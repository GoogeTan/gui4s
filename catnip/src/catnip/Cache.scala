package catnip

trait Cache[IO[_], K, V]:
  def get(key : K) : IO[Option[V]]

  def getOrPut(key : K, value : IO[V]) : IO[V]
end Cache
