package me.katze.gui4s.draw
package lwjgl

import cats.effect.Resource

trait Lwjgl[F[_]]:
  type Texture
  type Capabilities
  
  def createCapabilities : Resource[F, Capabilities]
  
  
end Lwjgl
  
