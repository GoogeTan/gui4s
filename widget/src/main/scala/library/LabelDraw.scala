package me.katze.gui4s.widget
package library

trait LabelDraw[+Draw, -LabelPlacementMeta]:
  def drawString(text : String, meta: LabelPlacementMeta) : Draw
end LabelDraw
