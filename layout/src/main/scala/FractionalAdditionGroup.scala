package me.katze.gui4s.layout

import cats.Group

given FractionalAdditionGroup[T : Fractional as F] : Group[T] with
  override def empty: T = F.zero

  override def combine(x: T, y: T): T = F.plus(x, y)

  override def inverse(a: T): T = F.negate(a) 
end FractionalAdditionGroup

  
