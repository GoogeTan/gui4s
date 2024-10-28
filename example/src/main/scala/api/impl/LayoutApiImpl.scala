package me.katze.gui4s.example
package api.impl

import api.{AdditionalAxisPlacementStrategy, LayoutApi, MainAxisPlacementStrategy}

import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout
import me.katze.gui4s.layout.Axis
import me.katze.gui4s.widget.Widget
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{*, given}

import scala.math.Fractional.Implicits.given

type LayoutPlacement[Update[+_, +_], Draw, Place[+_], Recompose, DownEvent, MU] =
    [Event]
        => (Axis, List[Place[Widget[Update, Draw, Place, Recompose, Event, DownEvent]]], MainAxisPlacementStrategy[MU], AdditionalAxisPlacementStrategy) 
        => Place[List[(Widget[Update, Draw, Place, Recompose, Event, DownEvent], LayoutPlacementMeta[MU])]]

trait LayoutApiImpl[Update[+_, +_], Draw, Place[+_] : Functor, Recompose, DownEvent, -MU : Fractional](
    using val lib : LayoutLibrary[Place, [A] =>> Widget[Update, Draw, Place, Recompose, A, DownEvent], LayoutPlacementMeta[MU]]
)(
  placement : LayoutPlacement[Update, Draw, Place, Recompose, DownEvent, MU]
) extends LayoutApi[MU]:

  final override type Widget[+Event] = Place[widget.Widget[Update, Draw, Place, Recompose, Event, DownEvent]]

  override def column[Event](
                              children: List[Widget[Event]],
                              verticalStrategy : MainAxisPlacementStrategy[MU],
                              horizontalStrategy : AdditionalAxisPlacementStrategy
                            ): Widget[Event] =
    linearLayout(
      children = children,
      axis = Axis.Vertical,
      mainAxisStrategy = verticalStrategy,
      additionalAxisStrategy = horizontalStrategy,
    )
  end column

  override def row[Event](
                            children: List[Widget[Event]],
                            horizontalStrategy : MainAxisPlacementStrategy[MU],
                            verticalStrategy : AdditionalAxisPlacementStrategy
                          ): Widget[Event] =
    linearLayout(
      children = children,
      axis = Axis.Horizontal,
      mainAxisStrategy = horizontalStrategy,
      additionalAxisStrategy = verticalStrategy,
    )
  end row

  private def linearLayout[Event](
                                children              : List[Widget[Event]],
                                axis                  : Axis,
                                mainAxisStrategy      : MainAxisPlacementStrategy[MU],
                                additionalAxisStrategy: AdditionalAxisPlacementStrategy,
                              ) : Widget[Event] =
    lib.layout(children, placement[Event](axis, _, mainAxisStrategy, additionalAxisStrategy))
  end linearLayout
end LayoutApiImpl
