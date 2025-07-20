package me.katze.gui4s.widget
package free

type AsFreeF[Placed, F[_]] = AsFree[Placed, F[Placed]]

type AsFree[-Placed, +Free] = (self : Placed) => Free