package me.katze.gui4s.example
package place

type RunPlacement[PlacementEffect[_], F[_]] = [Value] => PlacementEffect[Value] => F[Value]
