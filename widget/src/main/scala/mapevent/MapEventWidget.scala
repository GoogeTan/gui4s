package me.katze.gui4s.widget
package mapevent

import catnip.BiMonad
import catnip.syntax.bimonad.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monoid}
import me.katze.gui4s.widget.Widget

final case class MapEventWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : Functor,
  Recomposition : Monoid,
  DownEvent,
  A, B
](
  body : Widget[[Value] =>> Update[Value, A], Draw, Place, Recomposition, DownEvent], 
  f : A => B
) extends Widget[[Value] =>> Update[Value, B], Draw, Place, Recomposition, DownEvent]:

  override def handleDownEvent(pathToParent: Path, event: DownEvent): Update[Place[Widget[[Value] =>> Update[Value, B], Draw, Place, Recomposition, DownEvent]], B] =
    body.handleDownEvent(pathToParent, event).map(_.map(copy(_))).
      mapSecond(f)
  end handleDownEvent

  override def asUnplaced: Place[Widget[[Value] =>> Update[Value, B], Draw, Place, Recomposition, DownEvent]] =
    body.asUnplaced.map(copy(_))
  end asUnplaced

  override def mergeWithState(pathToParent: Path, oldState: Map[String, StateTree[Recomposition]]): Place[Widget[[Value] =>> Update[Value, B], Draw, Place, Recomposition, DownEvent]] =
    body.mergeWithState(pathToParent, oldState).map(copy(_))
  end mergeWithState

  override def childrenStates: Map[String, StateTree[Recomposition]] = body.childrenStates

  override def draw: Draw = body.draw

  override def recomposed(currentPath: Path, states: Map[String, StateTree[Recomposition]]): Recomposition =
    body.recomposed(currentPath, states)
  end recomposed
end MapEventWidget

object MapEventWidget:
  def apply[
    Update[+_, +_] : BiMonad,
    Draw,
    Place[+_] : Functor,
    Recomposition : Monoid,
    DownEvent,
    A, B
  ](body : Place[Widget[[W] =>> Update[W, A], Draw, Place, Recomposition, DownEvent]], f : A => B) : Place[MapEventWidget[Update, Draw, Place, Recomposition, DownEvent, A, B]] =
    body.map(MapEventWidget(_, f))
  end apply
end MapEventWidget
