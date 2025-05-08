package me.katze.gui4s.example
package place


trait RunPlacement[+F[_], PlacementEffect[_]]:
  def run[Value](toPalce: PlacementEffect[Value]) : F[Value]
  
  extension[Value](value : PlacementEffect[Value])
    def runPlacement : F[Value] = run(value)
end RunPlacement
