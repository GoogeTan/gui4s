package me.katze.gui4s.draw
package lwjgl

import org.lwjgl.*
import org.lwjgl.glfw.*
import org.lwjgl.glfw.Callbacks.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

@SuppressWarnings(Array("org.wartremover.warts.All"))
object Example extends App:
  // The window handle
  private var window: Long = 0L
  println("Hello LWJGL " + Version.getVersion + "!")
  init()
  loop()
  // Free the window callbacks and destroy the window
  glfwFreeCallbacks(window)
  glfwDestroyWindow(window)
  // Terminate GLFW and free the error callback
  glfwTerminate()
  glfwSetErrorCallback(null).free()

  private def init(): Unit =
    // Setup an error callback. The default implementation
    // will print the error message in System.err.
    GLFWErrorCallback.createPrint(System.err).set
    // Initialize GLFW. Most GLFW functions will not work before doing this.
    if (!glfwInit)
      throw new IllegalStateException("Unable to initialize GLFW")
    end if
    // Configure GLFW
    glfwDefaultWindowHints() // optional, the current window hints are already the default

    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE) // the window will stay hidden after creation

    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE) // the window will be resizable

    // Create the window
    window = glfwCreateWindow(300, 300, "Hello World!", NULL, NULL)
    if (window == NULL)
      throw new RuntimeException("Failed to create the GLFW window")
    end if

    // Setup a key callback. It will be called every time a key is pressed, repeated or released.
    glfwSetKeyCallback(window, (window, key, scancode, action, mods) =>
      if ((key eq GLFW_KEY_ESCAPE) && (action eq GLFW_RELEASE))
        glfwSetWindowShouldClose(window, true) // We will detect this in the rendering loop
      end if
    )
    // Get the thread stack and push a new frame
    try
      val stack = stackPush
      try
        val pWidth = stack.mallocInt(1) // int*

        val pHeight = stack.mallocInt(1) // int*

        // Get the window size passed to glfwCreateWindow
        glfwGetWindowSize(window, pWidth, pHeight)
        // Get the resolution of the primary monitor
        val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor)
        // Center the window
        glfwSetWindowPos(window, (vidmode.width - pWidth.get(0)) / 2, (vidmode.height - pHeight.get(0)) / 2)
      finally
        if (stack != null) stack.close()
      end try
    catch
      case thr => println(thr)
    end try
    // the stack frame is popped automatically

    // Make the OpenGL context current
    glfwMakeContextCurrent(window)
    // Enable v-sync
    glfwSwapInterval(1)
    // Make the window visible
    glfwShowWindow(window)
  end init
  
  private def loop(): Unit =
  {
    GL.createCapabilities
    val vertices = Array(0.0f, 0.5f, 0.0f, -0.5f, -0.5f, 0.0f, 0.5f, -0.5f, 0.0f)
    val vertexBuffer = memAllocFloat(vertices.length)
    vertexBuffer.put(vertices).flip
    while (!glfwWindowShouldClose(window))
    {
      glfwPollEvents()
      glClear(GL_COLOR_BUFFER_BIT)
      
      
      
      glColor3f(0.0f, 1.0f, 0.0f)
      glEnableClientState(GL_VERTEX_ARRAY)
      glVertexPointer(3, GL_FLOAT, 0, vertexBuffer)
      glDrawArrays(GL_TRIANGLES, 0, 3)
      glDisableClientState(GL_VERTEX_ARRAY)
      glfwSwapBuffers(window)
    }
    memFree(vertexBuffer)
    glfwDestroyWindow(window)
    glfwTerminate()
  }
end Example
