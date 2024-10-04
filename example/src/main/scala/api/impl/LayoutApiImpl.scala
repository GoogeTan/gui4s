package me.katze.gui4s.example
package api.impl

import api.{AdditionalAxisStrategy, LayoutApi, MainAxisStrategy}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget.library.lowlevel.WidgetLibrary
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.TaskFinished

import scala.math.Fractional.Implicits.given

trait LayoutApiImpl[-MU : Fractional](
    using val wl: WidgetLibrary
)(
  using val lib : LayoutLibrary[wl.type, LayoutPlacementMeta[MU]]
)(
  placement : [Event] => (Axis, List[wl.Widget[Event]], MainAxisStrategy[MU], AdditionalAxisStrategy) => wl.PlacementEffect[List[(wl.PlacedWidget[Event, wl.SystemEvent], LayoutPlacementMeta[MU])]]
) extends LayoutApi[MU]:

  given Functor[wl.PlacementEffect] = wl.placementIsEffect

  override type WidgetTask[+A] = wl.WidgetTask[A]
  override type Widget[+A] = wl.Widget[A]

  override def column[Event](
                              children: List[Widget[Event]],
                              verticalStrategy : MainAxisStrategy[MU],
                              horizontalStrategy : AdditionalAxisStrategy
                            ): Widget[Event] =
    rowColumn(
      children = children,
      axis = Axis.Vertical,
      mainAxisStrategy = verticalStrategy,
      additionalAxisStrategy = horizontalStrategy,
    )
  end column

  override def row[Event](
                            children: List[wl.Widget[Event]],
                            horizontalStrategy : MainAxisStrategy[MU],
                            verticalStrategy : AdditionalAxisStrategy
                          ): wl.Widget[Event] =
    rowColumn(
      children = children,
      axis = Axis.Horizontal,
      mainAxisStrategy = horizontalStrategy,
      additionalAxisStrategy = verticalStrategy,
    )
  end row

  private def rowColumn[Event](
                                children              : List[Widget[Event]],
                                axis                  : Axis,
                                mainAxisStrategy      : MainAxisStrategy[MU],
                                additionalAxisStrategy: AdditionalAxisStrategy,
                              ) : Widget[Event] =
    lib.layout(children, placement[Event](axis, _, mainAxisStrategy, additionalAxisStrategy))
  end rowColumn
end LayoutApiImpl
