package me.katze.gui4s.draw
package io

import java.nio.ByteBuffer

trait Resource[F[_]]:
  def resourceToByteBuffer(path : String, bufferSize : Int) : F[ByteBuffer]
end Resource

