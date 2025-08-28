package gui4s.core.widget

import cats.*
import cats.syntax.all.*

final case class Path(value : List[String]):
  def this(strings : String*) = this(strings.toList)
  
  def appendLast(s : String) : Path = Path(s :: value)
  def appendFirst(s : String) : Path = Path(value ++ List(s))
end Path

object Path:
  def unapplySeq(value : Path) : Option[Seq[String]] =
    Some(value.value)
  end unapplySeq

  given Eq[Path] = Eq.by(_.value)
end Path
