package me.katze.gui4s.layout

import cats.Group

given FractionalAdditionGroup[NumberT : Fractional as F] : Group[NumberT] with
  override def empty: NumberT = F.zero

  override def combine(x: NumberT, y: NumberT): NumberT = F.plus(x, y)

  override def inverse(a: NumberT): NumberT = F.negate(a) 
end FractionalAdditionGroup

  
