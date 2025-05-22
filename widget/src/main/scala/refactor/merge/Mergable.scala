package me.katze.gui4s.widget
package refactor.merge

import cats.data.NonEmptyList

trait Mergable[Place[_], Widget]:
  final def merge(head: Place[Widget], tail: Place[Widget]*): Place[Widget] =
    merge(NonEmptyList.of(head, tail *))
  end merge

  def merge(values: NonEmptyList[Place[Widget]]): Place[Widget] 
end Mergable

