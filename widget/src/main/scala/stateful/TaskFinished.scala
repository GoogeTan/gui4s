package me.katze.gui4s.widget
package stateful

final case class TaskFinished(currentWidget : String, furtherPath : List[String], value : Any):
  def this(path : Path, value : Any) = this(path.value.head, path.value.tail, value)
end TaskFinished

  
object TaskFinished:
  def apply(path : Path, value : Any): TaskFinished = TaskFinished(path.value.head, path.value.tail, value)
end TaskFinished