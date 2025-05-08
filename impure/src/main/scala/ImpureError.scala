package me.katze.gui4s.impure

trait ImpureError[F[_], Error]:
  def impureTry[Value](from : => Either[Error, Value]) : F[Value] 
end ImpureError

