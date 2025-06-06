package me.katze.gui4s.widget
package merge

import cats.data.NonEmptyList

trait Mergable[Place[_], Widget]:
  def merge(pathToWidget : Path, head: Widget, tail: Place[Widget]*): Place[Widget]
end Mergable

