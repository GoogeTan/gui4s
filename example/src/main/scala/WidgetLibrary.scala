package me.katze.gui4s.example

import cats.effect.*
import cats.*
import cats.syntax.all.{*, given}
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.example.Example
import me.katze.gui4s.example.impl.{FreeStatefulFabricImpl, WidgetTaskImpl}
import me.katze.gui4s.example.placeable.{*, given }
import me.katze.gui4s.example.stateful.{EventReaction, Mergeable, RichTypeChecker, State, TaskFinished}
import me.katze.gui4s.example.stateful

trait WidgetLibrary[IO[+_]]:
  type Widget[+Event]
  type WidgetTask[+T]

  def stateful[State, ParentEvent, ChildEvent](
                                                name : String,
                                                initialState : State,
                                                eventHandler : (State, ChildEvent) => EventReaction[WidgetTask[ChildEvent], State, ChildEvent, ParentEvent],
                                                render : State => Widget[ChildEvent]
                                              )(using RichTypeChecker[ChildEvent]) : Widget[ParentEvent]

  def row[Event](children : Widget[Event]*) : Widget[Event]
  def column[Event](children : Widget[Event]*) : Widget[Event]

  def textInput[Event](value : String, onUpdate : String => Event) : Widget[Event]

  def label(text : String) : Widget[Nothing]
end WidgetLibrary

