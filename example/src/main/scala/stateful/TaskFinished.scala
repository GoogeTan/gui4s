package me.katze.gui4s.example
package stateful

final case class TaskFinished(currentWidget : String, furtherPath : List[String], value : Any)