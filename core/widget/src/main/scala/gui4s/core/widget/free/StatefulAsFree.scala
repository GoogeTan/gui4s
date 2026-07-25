package gui4s.core.widget
package free

import catnip.CodensityT
import cats.Functor
import cats.syntax.functor.*
import gui4s.core.widget.merge.UpdateWidgetStateFromTheOldOne

def statefulAsFree[
  Place[_] : Functor,
  Widget,
  State,
](
  widgetAsFree : AsFree[Widget, Place[Widget]],
  stateAsFree : State => CodensityT[Place, Option[(State, Place[Widget])]],
  widgetsAreMergable  : UpdateWidgetStateFromTheOldOne[Place, Widget],
) : AsFree[Stateful[Widget, State], Place[Stateful[Widget, State]]] =
  self =>
    stateAsFree(self.stateBehaviour).f:
      case Some((newState, newChild)) =>
        widgetsAreMergable.mergeOldAndRerenderedWidgets(
          self.child,
          newChild
        )
          .map(child =>
            self.copy(
              stateBehaviour = newState,
              child = child
            )
          )
      case None =>
        widgetAsFree(self.child)
          .map(child =>
            self.copy(
              child = child
            )
          )
end statefulAsFree