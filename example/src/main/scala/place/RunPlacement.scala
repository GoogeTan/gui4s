package me.katze.gui4s.example
package place


trait RunPlacement[+F[_], PlacementEffect[_]]:
  def run[T](toPalce: PlacementEffect[T]) : F[T]
  
  extension[T](value : PlacementEffect[T])
    def runPlacement : F[T] = run(value)
end RunPlacement
