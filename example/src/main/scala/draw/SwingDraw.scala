package me.katze.gui4s.example
package draw

import cats.effect.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.example.place.ApplicationBounds

import java.awt.image.BufferedImage
import java.awt.{Color, Frame, Graphics}
import javax.swing.{JComponent, JFrame}
import me.katze.gui4s.example.update.*
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}

def initSwing : IO[SwingApi] =
  IO:
    val frame = new JFrame("Image Drawing Component")
    frame.setSize(400, 400)
    val comp = new SwingWindowComponent()
    comp.setSize(400, 400)
    frame.add(comp)
    frame.setVisible(true)
    
    
    new SwingApi:
      override def setSize(option: Option[(Int, Int)]): IO[Unit] =
        option match
          case Some((w, h)) =>
            IO:
              frame.setExtendedState(Frame.NORMAL)
              frame.setUndecorated(false)
              frame.setSize(w, h)
              comp.setSize(w, h)
          case None =>
            IO:
              frame.setExtendedState(Frame.MAXIMIZED_BOTH)
              frame.setUndecorated(true)
        end match
      end setSize
      
      override def graphics: SimpleDrawApi[IO] =
        SwingDraw(comp)
      end graphics

      override def size: IO[(Int, Int)] =
        IO:
          (frame.getWidth, frame.getHeight)
      end size
    end new
end initSwing

trait SwingApi:
  def setSize(option: Option[(Int, Int)]) : IO[Unit]
  def size : IO[(Int, Int)]
  def graphics : SimpleDrawApi[IO]
end SwingApi

class SwingProcessRequest(window : SwingApi) extends ProcessRequest[IO, ApplicationRequest]:
  extension (request: ApplicationRequest)
    override def process: IO[Option[ExitCode]] =
      request match
        case ApplicationRequest.BecomeFullScreen    => window.setSize(None) *> None.pure[IO]
        case ApplicationRequest.BecomeNotFullScreen => window.setSize(Some(500, 500)) *> None.pure[IO]
        case ApplicationRequest.CloseApp(code)      => Some(code).pure[IO]
      end match
    end process
  end extension
end SwingProcessRequest

class SwingApplicationBounds(api : SwingApi) extends ApplicationBounds[IO, Bounds[Int]]:
  override def currentBounds: IO[Bounds[Int]] =
    api.size.map((width, height) => Bounds(
      AxisBounds(None, Some(width)),
      AxisBounds(None, Some(height))
    ))
  end currentBounds
end SwingApplicationBounds

final class SwingDraw(
                        g : SwingWindowComponent
                      ) extends SimpleDrawApi[IO]:
  override def rectangle(x: Int, y: Int, width: Int, height: Int, color : Int): IO[Unit] =
    IO:
      g.graphics.setColor(Color(color))
      g.graphics.fillRect(x, y, width, height)
  end rectangle
  
  
  override def text(x: Int, y: Int, text: String, style: TextStyle): IO[Unit] =
    IO:
      g.graphics.setColor(Color(style.color))
      g.graphics.drawString(text, x, y)
  end text
  
  override def endDraw: IO[Unit] =
    IO:
      g.repaint()
  end endDraw
  
  override def beginDraw: IO[Unit] =
    IO:
      g.setImage(new BufferedImage(g.getWidth, g.getHeight, BufferedImage.TYPE_INT_ARGB))
  end beginDraw
end SwingDraw

class SwingWindowComponent extends JComponent:
  private[draw] var image: BufferedImage = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
  private[draw] var graphics = image.createGraphics()
  
  def setImage(newImage : BufferedImage) : Unit =
    image = newImage
    graphics = newImage.createGraphics()
  end setImage
  
  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    g.drawImage(image, 0, 0, null)
    graphics.drawImage(image, 0, 0, null)
  end paintComponent
end SwingWindowComponent