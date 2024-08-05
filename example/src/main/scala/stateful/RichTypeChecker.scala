package me.katze.gui4s.example
package stateful

trait RichTypeChecker[+M]:
  def tryCast(value: Any) : Either[String, M]
