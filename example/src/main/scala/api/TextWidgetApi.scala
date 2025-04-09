package me.katze.gui4s.example
package api

trait TextWidgetApi[-TextStyle] extends HighLevelApi:
  def text(text : String, style : TextStyle) : Widget[Nothing]
  
