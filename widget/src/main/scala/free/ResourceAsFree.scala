package me.katze.gui4s.widget
package free

import cats.Functor
import cats.syntax.all.*

def resourceAsFree[Widget, Value, Place[_] : Functor](widgetAsFree : AsFreeF[Widget, Place]) : AsFreeF[Resource[Widget, Value], Place] =
  resource =>
    widgetAsFree(resource.widget).map(newWidget => resource.copy(widget = newWidget))
end resourceAsFree
