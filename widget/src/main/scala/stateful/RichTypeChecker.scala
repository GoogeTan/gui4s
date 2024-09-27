package me.katze.gui4s.widget
package stateful

trait RichTypeChecker[+M]:
  def tryCast(value: Any) : Either[String, M]
