package gui4s.core.widget

import cats._

final case class Path(value : List[String]):
  def this(strings : String*) = this(strings.toList)

  def /(s : String) : Path =
    Path(s :: value)
  end /

  override def toString: String =
    value.reverse.mkString("::")
end Path

object Path:
  def unapplySeq(value : Path) : Option[Seq[String]] =
    Some(value.value)
  end unapplySeq

  given Eq[Path] = Eq.by(_.value)
end Path
