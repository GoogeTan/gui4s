package me.katze.gui4s.draw
package lwjgl.test

import cats.effect.IO
import cats.effect.kernel.Resource
import me.katze.gui4s.draw.lwjgl.IOUtil
import org.lwjgl.BufferUtils
import org.lwjgl.BufferUtils.createByteBuffer
import org.lwjgl.system.MemoryUtil.memSlice

import java.io.{IOException, InputStream}
import java.net.URL
import java.nio.ByteBuffer
import java.nio.channels.{Channels, ReadableByteChannel, SeekableByteChannel}
import java.nio.file.{Files, Path, Paths}

/**
 * Reads the specified resource and returns the raw data as a ByteBuffer.
 *
 * @param resource   the resource to read
 * @param bufferSize the initial buffer size
 * @return the resource data
 * @throws IOException if an IO error occurs
 */
@SuppressWarnings(Array("org.wartremover.warts.All"))
@throws[IOException]
def ioResourceToByteBuffer(resource: String, bufferSize: Int) : IO[ByteBuffer] =
  val path = if (resource.startsWith("http")) null else Paths.get(resource)
  if path != null && Files.isReadable(path) then
    newByteChannel(path).use:
      fc =>
        IO:
          var buffer: ByteBuffer = null
          try
            buffer = BufferUtils.createByteBuffer(fc.size.toInt + 1)
            while (fc.read(buffer) != -1){}
          finally
            if (fc != null) fc.close()
          end try
          buffer.flip
          memSlice(buffer)
  else
    source(resource).flatMap(channel).use:
      rbc =>
        IO:
          var buffer: ByteBuffer = createByteBuffer(bufferSize)
          var shouldBreak = false
          while !shouldBreak do
            val bytes = rbc.read(buffer)
            if bytes == -1 then
              shouldBreak = true
            else
              if buffer.remaining == 0 then
                buffer = resizeBuffer(buffer, buffer.capacity * 3 / 2) // 50%
              end if
            end if
          end while
          buffer.flip
          memSlice(buffer)
end ioResourceToByteBuffer

def newByteChannel(path : Path) : Resource[IO, SeekableByteChannel] =
  Resource.fromAutoCloseable(IO.apply(Files.newByteChannel(path)))
end newByteChannel

def source(resource : String) : Resource[IO, InputStream] =
  Resource.fromAutoCloseable(
    IO:
      if (resource.startsWith("http"))
        new URL(resource).openStream
      else
        classOf[IOUtil].getClassLoader.getResourceAsStream(resource)
  )
end source

def channel(source : InputStream) : Resource[IO, ReadableByteChannel] =
  Resource.fromAutoCloseable(
    IO:
      Channels.newChannel(source)
  )
end channel

def resizeBuffer(buffer: ByteBuffer, newCapacity: Int) =
  val newBuffer = BufferUtils.createByteBuffer(newCapacity)
  buffer.flip
  newBuffer.put(buffer)
  newBuffer
end resizeBuffer
