package me.katze.gui4s.example
package task


type EventProducingEffect[F[_], +T] = (T => F[Unit]) => F[Unit]
type EventProducingEffectT[F[_]] = [T] =>> EventProducingEffect[F, T]