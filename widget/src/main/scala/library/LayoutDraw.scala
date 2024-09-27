package me.katze.gui4s.widget
package library


trait LayoutDraw[Draw, ChildrenMeta]:
  def drawChildren(children : List[(Draw, ChildrenMeta)]) : Draw
end LayoutDraw
