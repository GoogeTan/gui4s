package me.katze.gui4s.example
package api

trait LabelApi[-TextStyle] extends HighLevelApi:
  def label(text : String, style : TextStyle) : Widget[Nothing]
  
