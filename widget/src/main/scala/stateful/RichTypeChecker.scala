package me.katze.gui4s.widget
package stateful

trait RichTypeChecker[+M]:
  def tryCast(value: Any, errorText : String) : M
