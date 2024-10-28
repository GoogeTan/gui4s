package me.katze.gui4s.widget
package stateful

final case class Path(value : List[String] = Nil):
  def this(strings : String*) = this(strings.toList)
  
  def appendLast(s : String) : Path = Path(s :: value)
  def appendFirst(s : String) : Path = Path(value ++ List(s))
  def last: String = value.last
  def first: String = value.head

object Path:
  def unapplySeq(value : Path) : Option[Seq[String]] =
    Some(value.value)