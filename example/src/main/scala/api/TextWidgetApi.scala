package me.katze.gui4s.example
package api

trait TextWidgetApi[Widget, -TextStyle]:
  def text(text : String, style : TextStyle) : Widget
  
