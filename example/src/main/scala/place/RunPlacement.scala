package me.katze.gui4s.example
package place


trait RunPlacement[+F[_], PlacementEffect[_]]:
  def run[T](toPalce: PlacementEffect[T]) : F[T]
end RunPlacement
