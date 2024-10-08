package me.katze.gui4s.draw
package lwjgl

import fs2.Chunk.ByteBuffer
import org.lwjgl.*
import org.lwjgl.glfw.*
import org.lwjgl.opengl.*
import org.lwjgl.system.*
import org.lwjgl.glfw.Callbacks.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.stb.STBEasyFont
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.stb.*

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
    // This line is critical for LWJGL's interoperation with GLFW's
    // OpenGL context, or any context that is managed externally.
    // LWJGL detects the context that is current in the current thread,
    // creates the GLCapabilities instance and makes the OpenGL
    // bindings available for use.
    GL.createCapabilities
    // Set the clear color
    glClearColor(1.0f, 1.0f, 0.0f, 0.0f)
    // Run the rendering loop until the user has attempted to close
    // the window or has pressed the ESCAPE key.
    while (!glfwWindowShouldClose(window))
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT) // clear the framebuffer

      glfwSwapBuffers(window) // swap the color buffers

      // Poll for window events. The key callback above will only be
      // invoked during this call.
      glfwPollEvents()
    end while
  end loop
end Example
