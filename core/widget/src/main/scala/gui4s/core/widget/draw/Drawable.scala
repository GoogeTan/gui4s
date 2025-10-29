package gui4s.core.widget
package draw

import cats.Contravariant

type Drawable[-T, +Draw] = (self : T) => Draw

given drawableIsContravariant[Draw] : Contravariant[Drawable[*, Draw]] with
  override def contramap[A, B](fa: Drawable[A, Draw])(f: B => A): Drawable[B, Draw] =
    f.andThen(fa)
  end contramap
end drawableIsContravariant