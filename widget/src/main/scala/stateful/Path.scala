package me.katze.gui4s.widget
package stateful

final case class Path(value : List[String]):
  def this(strings : String*) = this(strings.toList)
  
  def appendLast(s : String) : Path = Path(s :: value)
  def appendFirst(s : String) : Path = Path(value ++ List(s))
end Path

object Path:
  def unapplySeq(value : Path) : Option[Seq[String]] =
    Some(value.value)