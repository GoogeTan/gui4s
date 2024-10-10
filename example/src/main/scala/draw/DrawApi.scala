package me.katze.gui4s.example
package draw

import api.impl.DrawMonadT

trait DrawApi[F[_], MU : Numeric]:
  val window : Window[F, MU]
  def graphics[Draw[_] : DrawMonadT[MU]](using Lift[F, Draw, (MU, MU)]) : SimpleDrawApi[MU, Draw[Unit]]
end DrawApi

