package me.katze.gui4s.example
package api

trait TextWidgetApi[Widget, -Shaper, -TextStyle]:
  def text(text : String, shaper : Shaper, style : TextStyle) : Widget
  
