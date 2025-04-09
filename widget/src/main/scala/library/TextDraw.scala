package me.katze.gui4s.widget
package library

trait TextDraw[+Draw, -TextPlacementMeta]:
  def drawString(text : String, meta: TextPlacementMeta) : Draw
end TextDraw
